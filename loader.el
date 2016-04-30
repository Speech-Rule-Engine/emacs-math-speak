;;; Currently testing!

(defvar ems-node-buffer nil)
(defvar ems-node-process nil)
(defvar ems-output-buffer nil)
(defvar ems-request-counter 0)

(setq inferior-js-mode-hook
      #'(lambda ()
        (add-to-list
         'comint-preoutput-filter-functions
         #'(lambda (output)
           (string-match (format "BEGINOUTPUT" ems-request-counter) output))
           (replace-regexp-in-string "[.]*BEGINOUTPUT[0-9]+" "" output)
           (replace-regexp-in-string "ENDOUTPUT[.]*" "" output))))

(defun ems-load-node ()
  (run-js inferior-js-program-command t)
  (setq ems-node-buffer (get-buffer "*js*"))
  (setq ems-node-process (get-buffer-process "*js*"))
  (let ((output-buffer (get-buffer "*ems-output*")))
    (if output-buffer
        (setq ems-output-buffer output-buffer)
      (setq ems-output-buffer (generate-new-buffer "*ems-output*"))
      )))

(defun ems-load-mathjax ()
  (comint-redirect-send-command-to-process
   "var mjx = require('mathjax-node');" ems-output-buffer ems-node-buffer t))

(defun ems-load-sre ()
  (comint-redirect-send-command-to-process
   "var sre = require('speech-rule-engine');" ems-output-buffer ems-node-buffer t))


;; Input: LaTeX expression
(defun ems-start-walker (expr)
  (comint-redirect-send-command-to-process
   (format "mjx.typeset({math: '%s', format: 'TeX', mml:true}, function(data) {sre.walk(data.mml)});" expr)
   ems-output-buffer ems-node-buffer t))

;; Input: A key value
(defun ems-move-walker (key)
  (let* ((counter (incf ems-request-counter))
         (command (format "console.log('BEGINOUTPUT%d:' + sre.move(%d) + 'ENDOUTPUT%d');"
             counter key counter)))
    (comint-redirect-send-command-to-process
     command
     ems-output-buffer ems-node-buffer t)
    (ems-parse-output-buffer counter)))


;; TODO Error handling after each step.
(defun ems-setup-bridge ()
  (ems-load-node)
  (ems-load-mathjax)
  (ems-load-sre))

(defun ems-teardown-bridge ()
  (kill-buffer ems-output-buffer)
  (setf ems-output-buffer nil)
  (setf ems-node-buffer nil))


;; This does not work!
;; Number.
;; Return: The relevant output in the buffer.
(defun ems-parse-output-buffer (counter)
  (accept-process-output ems-node-process 1)
  (with-current-buffer ems-output-buffer
    (let* ((str (buffer-string))
           (start (string-match (format "BEGINOUTPUT%d" counter) str))
           (end (string-match (format "ENDOUTPUT%d" counter) str)))
      (if (and start end) (subseq str (+ start 12) end))
      )))


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

