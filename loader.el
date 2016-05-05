;;; Currently testing!
(require 'js-comint)
(require 'hydra)
(require 'emacspeak-muggles)

(defvar ems-node-buffer nil)
(defvar ems-node-process nil)
(defvar ems-output-buffer nil)
(defvar ems-request-counter 0)
(defvar ems-results ())

(add-hook
 'inferior-js-mode-hook
 #'(lambda ()
     (add-to-list
      'comint-preoutput-filter-functions
      #'(lambda (output)
          (with-current-buffer (get-buffer "*ems-output*")
            (let ((result (ems-parse-output output)))
              (when result
                (setq ems-results (append result ems-results))
                (mapcar #'(lambda (x)
                            (insert (cdr x))
                            (insert "\n"))
                        result))
              output))))))

(defun ems-load-node ()
  (run-js inferior-js-program-command t)
  (setq ems-node-buffer (get-buffer "*js*"))
  (setq ems-node-process (get-buffer-process "*js*"))
  (let ((output-buffer (get-buffer "*ems-output*")))
    (if output-buffer
        (setq ems-output-buffer output-buffer)
      (setq ems-output-buffer (generate-new-buffer "*ems-output*"))
      )))

(defun ems-load-setup ()
  (process-send-string
   ems-node-process
   (concat
    "var mjx = require('mathjax-node');"
    "var sre = require('speech-rule-engine');"
    "var runWithCounter = function(counter, callback, args) {"
    "  var result = callback.apply(sre, args);"
    "  console.log('BEGINOUTPUT' + counter + ': ' + result + ' :ENDOUTPUT');"
    "};\n"
    )))

(defun ems-next-counter ()
  (incf ems-request-counter))

;; Input: LaTeX expression
(defun ems-start-walker (expr)
  (process-send-string
   ems-node-process
                                        ;TODO: Here we can get LaTeX errors.
   (concat
    (format "mjx.typeset({math: '%s', format: 'TeX', mml:true}, " expr)
    (format "function(data) {sre.walk(data.mml)});\n")
    )))

;; Input: A key value
(defun ems-move-walker (key)
  (process-send-string
   ems-node-process
   (format "runWithCounter(%d, sre.move, [%d]);\n" (incf ems-request-counter) key)))

;; TODO Error handling after each step.
(defun ems-setup-bridge ()
  (ems-load-node)
  (ems-load-setup))

(defun ems-teardown-bridge ()
  (kill-buffer ems-output-buffer)
  (setq  ems-request-counter 0)
  (setf ems-output-buffer nil)
  (setf ems-node-buffer nil))

;; Number: The full buffer output.
;; Return: Cons with call number and result.
(defun ems-parse-output (output &optional acc)
  (let* ((start (string-match "BEGINOUTPUT[0-9]+: .* :ENDOUTPUT" output)))
    (if start
        (let* ((rest (subseq output (+ start 11)))
               (colon (string-match ": " rest))
               (number (subseq rest 0 colon))
               (rest (subseq rest (+ colon 2)))
               (end (string-match " :ENDOUTPUT" rest))
               (result (subseq rest 0 end))
               )
          ;;(push (cons (car (read-from-string number)) result) ems-results)
          (ems-parse-output (subseq rest end)
                            (cons (cons (car (read-from-string number)) result) acc)))
      acc)))

(defun ems-parameterize-engine (feature value)
  (process-send-string
   ems-node-process
   (format "sre.setupEngine({'%s': '%s'});\n" feature value)))


(defvar ems-domain-styles '((("default" . "chromevox") .
                             (("default" . "verbose") ("short" . "short")))
                            (("mathspeak" . "mathspeak") .
                             (("default" . "verbose") ("brief" . "brief") ("sbrief" . "superbrief")))))
(defvar ems-domain-counter 0)
(defvar ems-style-counter 1)

(defun ems-next-style ()
  (let* ((domain (nth ems-domain-counter ems-domain-styles))
         (style (nth (incf ems-style-counter) (cdr domain))))
    (unless style
      (setq ems-style-counter 0)
      (setq style (cadr domain)))
    (ems-parameterize-engine '(("style". (car style))))
    (concat "style " (cdr style))))

(defun ems-next-domain () 
  (let ((domain (nth (incf ems-domain-counter) ems-domain-styles)))
    (unless domain 
      (setq ems-domain-counter 0)
      (setq domain (car ems-domain-styles)))
    (setq ems-style-counter 0)
    (ems-parameterize-engine "domain" (caar domain))
    (ems-parameterize-engine "style"  (caadr domain))
    (concat "rules " (cdar domain)
            " style " (cdadr domain))))


;;; API
(defun ems-start ()
  (interactive)
  (ems-setup-bridge))
(defun ems-render-result ()
  "Render current result."
  (interactive)
  (ems-repeat)
  (while ; drain output
      (accept-process-output (get-buffer-process "*js*") 1 nil 'just-this-one)
    t)
  (when (featurep 'emacspeak)
    (dtk-speak-and-echo (cdar ems-results))))

(defun ems-enter (expr)
  (interactive "sLaTeX: ")
  (ems-start-walker expr)
  (ems-render-result))

(defun ems-up ()
  (interactive)
  (ems-move-walker 38)
  (ems-render-result))

(defun ems-down ()
  (interactive)
  (ems-move-walker 40)
  (ems-render-result))

(defun ems-left ()
  (interactive)
  (ems-move-walker 37)
  (ems-render-result))

(defun ems-right ()
  (interactive)
  (ems-move-walker 39)
  (ems-render-result))

(defun ems-repeat ()
  (interactive)
  (ems-move-walker 9))

(defun ems-depth ()
  (interactive)
  (ems-move-walker 32)
  (ems-render-result))

(defun ems-exit ()
  (interactive)
  (ems-move-walker 27)
  (ems-render-result))

(defun ems-stop ()
  (interactive)
  (ems-teardown-bridge))


(defun ems-toggle-rules ()
  (interactive)
  (dtk-speak-and-echo (ems-next-domain)))

(defun ems-toggle-style ()
  (interactive)
  (dtk-speak-and-echo (ems-next-style)))



(defun ems-test ()
  (ems-start)
  (ems-start-walker "\\\\frac{x}{y}")
  ;; That does not seem to actually wait!
  (sleep-for 2)
  (ems-repeat)
  (ems-down)
  (ems-right)
  )

;;; Hydra for interactive top-level call

(global-set-key
 (kbd "C-c m")
 (defhydra emaths-explore
   (:body-pre
    (progn
      (emacspeak-muggles-toggle-talkative)
      (emacspeak-muggles-body-pre "Math"))
    :pre emacspeak-muggles-pre
    :post emacspeak-muggles-post)
   "Maths"
   ("e" ems-enter "Expr")
   ("h" ems-left "Left")
   ("l" ems-right  "Right")
   ("j" ems-down "Down")
   ("k" ems-up "Up")
   ("x" ems-exit "Exit")
   ("SPC" ems-depth "Depth")
   ("C-i" ems-repeat "Repeat")
   ("d" ems-toggle-rules "Toggle Rules")
   ("f" ems-toggle-style "Toggle Style")
   ("s" emacspeak-muggles-toggle-talkative "Talkative")
   ("." ems-render-result "Current")))
