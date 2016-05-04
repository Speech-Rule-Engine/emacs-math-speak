;;; Currently testing!

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
    (format "function(data) {sre.walk(data.mml)});\n"))))

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

;;; API
(defun ems-start ()
  (ems-setup-bridge))

(defun ems-enter (expr)
  (ems-start-walker expr)
  (accept-process-output (get-buffer-process "*js*") 1)
  ;; Need to wait for callback!
  (ems-repeat 2)
  )

(defun ems-up ()
  (ems-move-walker 38))

(defun ems-down ()
  (ems-move-walker 40))

(defun ems-left ()
  (ems-move-walker 37))

(defun ems-right ()
  (ems-move-walker 39))

(defun ems-repeat ()
  (ems-move-walker 9))

(defun ems-depth ()
  (ems-move-walker 32))

(defun ems-exit ()
  (ems-move-walker 27))

(defun ems-stop ()
  (ems-teardown-bridge))

(defun ems-test ()
  (ems-start)
  (ems-start-walker "\\\\frac{x}{y}")
  ;; That does not seem to actually wait!
  (sleep-for 2)
  (ems-repeat)
  (ems-down)
  (ems-right)
  )
