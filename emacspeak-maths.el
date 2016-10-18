;;; emacspeak-maths.el --- Speak Mathematics -*-lexical-binding: t
;;; $Author: tv.raman.tv, zorkov  $
;;; Description:  Speak MathML and LaTeX math expressions
;;; Keywords: Emacspeak,  Audio Desktop maths
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-05-03 18:13:44 -0700 (Thu, 03 May 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:
;;;Copyright (C) 1995 -- 2007, 2011, T. V. Raman
;;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
;;; All Rights Reserved.
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNMATHS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  introduction

;;; Commentary:

;;; Spoken mathematics on the emacspeak audio desktop. Use a NodeJS
;;; based speech-rule-engine for Mathematics as the backend for
;;; processing mathematical markup. The result of this processing is
;;; an annotated S-expression that is rendered via Emacspeak's speech
;;; facilities. Annotations follow Aural CSS as implemented in
;;; Emacspeak, This allows us to map these expressions to aural
;;; properties supported by specific TTS engines. Basic Usage: Startup
;;; up the server/client: M-x emacspeak-maths-start. Once the server
;;; and client are started, you can browse any number of math
;;; expressions using the emacspeak-maths-navigator described below.
;;;
;;; Invoke the Navigator using s-spc --- this is the <windows> key on
;;; Linux. Now you can use these keys: @itemize @item Enter: <SPC>
;;; Enter a LaTeX expression. @item Alt-Text <a> Process alt-text
;;; under point as LaTeX. @item Down <down> Move down a level. @item
;;; Up <up> Move up a level. @item Left <left> Move left. @item Right
;;; <right> Move right. @item Exit <any other key> Exit
;;; navigator. @end itemize The current expression is spoken after
;;; each of the above commands. It is also displayed in a special
;;; buffer *Spoken Math*. That buffer holds all previously generated
;;; output, And Emacs commands forward-page and backward-page can be
;;; used to move through each chunk of output.

;;; Code:

;;}}}
;;{{{  Required modules

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'comint)
(require 'derived)
(cl-eval-when '(load)
  (when (locate-library "package")
    (unless (locate-library "hydra") (package-install 'hydra))))
(require 'hydra "hydra" 'no-error)
(require 'emacspeak-preamble)
(require 'emacspeak-muggles)

;;}}}
;;{{{ Customizations And Variables:

(defgroup emacspeak-maths nil
  "Customize Emacspeak  Maths."
  :group 'emacspeak)

(defcustom emacspeak-maths-inferior-program
  (executable-find "node")
  "Location of `node' executable."
  :type 'file
  :group 'emacspeak-maths)

(cl-defstruct emacspeak-maths
  server-buffer ; comint buffer
  server-process ; node process handle
  client-buffer ; network socket stream
  client-process ; network connection
  input  ; LaTeX we send
  output ; where output is displayed
  pause ; pending pause to add
  result
  )

(defvar emacspeak-maths nil
  "Structure holding all runtime context.")

;;}}}
;;{{{ Parser Setup:

(defvar emacspeak-maths-handler-table (make-hash-table :test #'eq)
  "Map of handlers for parsing Maths Server output.")
(defsubst emacspeak-maths-handler-set (name handler)
  "Set up handler for name `name'."
  (declare (special emacspeak-maths-handler-table))
  (puthash name handler emacspeak-maths-handler-table))

(defsubst emacspeak-maths-handler-get (name)
  "Return handler  for name `name'.
Throw error if no handler defined."
  (declare (special emacspeak-maths-handler-table))
  (or (gethash name emacspeak-maths-handler-table)
      (error "No handler defined for %s" name)))

;;}}}
;;{{{ Handlers:

;;; All handlers are called with the body of the unit being parsed.
;;; Handlers process input and render to output buffer
;;; Except for the pause handler that merely records the pause,
;;; Leaving it to the next text handler to consume that pause.

;;; Helper: Handle plain strings

(defun emacspeak-maths-handle-string (string)
  "Handle plain, unannotated string."
  (declare (special emacspeak-maths))
  (with-current-buffer (emacspeak-maths-output emacspeak-maths)
    (let ((start (point)))
      (insert (format "%s\n" string))
      (emacspeak-maths-apply-pause start))))

(defun emacspeak-maths-parse (sexp)
  "Top-level parser dispatch.
If sexp is a string, return it.
Otherwise, Examine head of sexp, and applies associated handler to the tail."
  (cond
   ((stringp sexp)
    (emacspeak-maths-handle-string sexp))
   (t
    (cl-assert  (listp sexp) t "%s is not a list." contents)
    (let ((handler (emacspeak-maths-handler-get(car sexp))))
      (cl-assert (fboundp handler) t "%s is not  a function.")
      (funcall handler (cdr sexp))))))

(defun emacspeak-maths-handle-exp (contents)
  "Handle top-level exp returned from Maths Server."
  (declare (special emacspeak-maths))
  (with-current-buffer (emacspeak-maths-output emacspeak-maths)
    (goto-char (point-max))
    (let ((inhibit-read-only  t)
          (start (point))
          (end nil))
      (mapc #'emacspeak-maths-parse contents)
      (setq end (point))
      (insert "\f")
      (goto-char start)
      (display-buffer (emacspeak-maths-output emacspeak-maths))
      (tts-with-punctuations 'some
                             (emacspeak-speak-region start end)))))

(defun emacspeak-maths-acss (acss-alist)
  "Return ACSS voice corresponding to acss-alist."
  (let-alist acss-alist
    (acss-personality-from-speech-style
     (make-acss
      :average-pitch  .average-pitch
      :pitch-range .pitch-range
      :stress .stress
      :richness .richness))))

;;;Helper: Apply pause and consume:

(defun emacspeak-maths-apply-pause (start)
  "Apply pause."
  (declare (special emacspeak-maths))
  (let ((pause (emacspeak-maths-pause emacspeak-maths)))
    (when pause
      (save-excursion
        (goto-char start)
        (skip-syntax-forward " ")
        (put-text-property
         (point) (1+ (point))
         'pause pause))
      (setf (emacspeak-maths-pause emacspeak-maths) nil))))

(defun emacspeak-maths-handle-text (contents)
  "Handle body of annotated text from Maths Server.
Expected: ((acss) string)."
  (declare (special emacspeak-maths))
  (cl-assert (listp contents) t "%s is not a list. " contents)
  (let ((acss (cl-first contents))
        (string (cl-second contents))
        (pause (emacspeak-maths-pause emacspeak-maths))
        (start nil))
    (with-current-buffer  (emacspeak-maths-output emacspeak-maths)
      (setq start (goto-char (point-max)))
      (insert (format "%s\n" string))
      (put-text-property
       start (point)
       'personality (emacspeak-maths-acss acss))
      (emacspeak-maths-apply-pause start))))

(defun emacspeak-maths-handle-pause (ms)
  "Handle Pause value."
  (declare (special emacspeak-maths))
  (cl-assert (numberp ms) t "%s is not a number. " ms)
  (cond
   ((null (emacspeak-maths-pause emacspeak-maths))
    (setf (emacspeak-maths-pause emacspeak-maths) ms))
   ((numberp (emacspeak-maths-pause emacspeak-maths))
    (cl-incf (emacspeak-maths-pause emacspeak-maths) ms))
   (t (error "Invalid pause %s set earlier."
             (emacspeak-maths-pause emacspeak-maths)))))

(defun emacspeak-maths-handle-error (contents)
  "Display error message."
  (message "%s" contents))

(defun emacspeak-maths-handle-parse-error (contents)
  "Display parse-error message."
  (message "%s" contents))

(defun emacspeak-maths-handle-welcome (contents)
  "Handle welcome message."
  (message "%s" contents))

;;}}}
;;{{{ Map Handlers:
(cl-loop
 for f in
 '(exp pause text error welcome parse-error)
 do
 (emacspeak-maths-handler-set
  f
  (intern (format "emacspeak-maths-handle-%s"  (symbol-name f)))))

;;}}}
;;{{{ Process Filter:

(defun emacspeak-maths-read-output ()
  "Parse and return one complete chunk of output. Throws an error on an
incomplete parse, that is expected to be caught by the caller."
   ;;; return first sexp and move point
  (emacspeak-maths-parse (read (current-buffer))))

(defun emacspeak-maths-process-filter (proc string)
  "Handle process output from Node math-server.
All complete chunks of output are consumed. Partial output is
left for next run."
  (declare (special emacspeak-maths))
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
;;; Insert the text, advancing the process marker.
        (goto-char (process-mark proc))
        (insert string)
        (set-marker (process-mark proc) (point)))
;;; Consume process output
      (save-excursion
        (goto-char (point-min))
        (flush-lines "^ *$")
        (goto-char (point-min))
        (skip-syntax-forward " >")
        (let((result nil)
             (start (point)))
          (condition-case nil
              (while (not (eobp))
;;; Parse one complete chunk
                (setq result (emacspeak-maths-read-output))
;;; Todo: reverse later depending on how we use it.
                (setf (emacspeak-maths-result emacspeak-maths) result)
                (skip-syntax-forward " >")
                (delete-region start (point))
                (setq start (point)))
            (error nil))))
      (if moving (goto-char (process-mark proc))))))

;;}}}
;;{{{ Setup:

(defvar emacspeak-maths-server-program
  (expand-file-name "math-server.js"
                    (file-name-directory (or load-file-name default-directory)))
  "NodeJS implementation of math-server.")
;;;###autoload

(defun emacspeak-maths-start ()
  "Start Maths server bridge."
  (interactive)
  (declare (special emacspeak-maths-inferior-program
                    emacspeak-maths emacspeak-maths-server-program))
  (let ((server
         (make-comint
          "Server-Maths" emacspeak-maths-inferior-program nil
          emacspeak-maths-server-program))
        (client nil))
    (accept-process-output (get-buffer-process server) 1.0 nil 'just-this-one)
    (setq client
          (open-network-stream "Client-Math" "*Client-Math*" "localhost" 5000))
    (setf emacspeak-maths
          (make-emacspeak-maths
           :output (emacspeak-maths-setup-output)
           :server-buffer  server
           :server-process (get-buffer-process server)
           :client-process client
           :client-buffer (process-buffer client)))
    (set-process-filter client #'emacspeak-maths-process-filter))
  (when (called-interactively-p 'interactive)
    (message "Started Maths server and client.")))

(defun emacspeak-maths-shutdown ()
  "Shutdown client and server processes."
  (interactive)
  (declare (special emacspeak-maths))
  (when (process-live-p (emacspeak-maths-client-process emacspeak-maths))
    (delete-process (emacspeak-maths-client-process emacspeak-maths)))
  (when (process-live-p (emacspeak-maths-server-process emacspeak-maths))
    (delete-process (emacspeak-maths-server-process emacspeak-maths)))
  (when (buffer-live-p (emacspeak-maths-server-buffer emacspeak-maths))
    (kill-buffer (emacspeak-maths-server-buffer emacspeak-maths)))
  (when (buffer-live-p (emacspeak-maths-client-buffer emacspeak-maths))
    (kill-buffer (emacspeak-maths-client-buffer emacspeak-maths)))
  (when (called-interactively-p 'interactive)
    (message "Shutdown Maths server and client.")))

(defun emacspeak-maths-ensure-server ()
  "Start up Maths Server bridge if not already running."
  (declare (special emacspeak-maths))
  (unless
      (and emacspeak-maths
           (process-live-p (emacspeak-maths-server-process emacspeak-maths))
           (process-live-p (emacspeak-maths-client-process emacspeak-maths)))
    (emacspeak-maths-start)))

(defun emacspeak-maths-restart ()
  "Restart Node math-server if running. Otherwise starts a new one."
  (interactive)
  (emacspeak-maths-shutdown)
  (emacspeak-maths-start)
  (message "Restarting Maths server and client."))

;;}}}
;;{{{ Navigators:

;;; Helper: Guess current math expression from TeX/LaTeX

(defun emacspeak-maths-guess-tex ()
  "Extract math content around point."
  (declare (special texmathp-why))
  (cl-assert (require 'texmathp) nil "Install package auctex to get texmathp")
  (when (texmathp)
    (let ((delimiter (car texmathp-why))
          (start (cdr texmathp-why))
          (begin nil)
          (end nil))
      (cond
;;; $ and $$
       ((or (string= "$" delimiter)
            (string= "$$" delimiter))
        (save-excursion
          (goto-char start)
          (forward-char (length delimiter))
          (setq begin (point))
          (skip-syntax-forward "^$")
          (setq end (point))
          (buffer-substring begin end)))
;;; \( and \[
       ((string= "\\(" delimiter)
        (goto-char start)
        (setq begin (+ start  2))
        (search-forward "\\)")
        (setq end (- (point) 2))
        (buffer-substring begin end))
       ((string= "\\[" delimiter)
        (goto-char start)
        (setq begin (+ start  2))
        (search-forward "\\]")
        (setq end (- (point) 2))
        (buffer-substring begin end))
;;; begin equation
       ((string= "equation" delimiter)
        (goto-char start)
        (forward-char (length "\\begin{equation}"))
        (setq begin (point))
        (search-forward "\\end{equation}")
        (backward-char (length "\\begin{equation}"))
        (setq end (point))
        (buffer-substring begin end))
                
       (t nil)))))

(defun emacspeak-maths-guess-input ()
  "Examine current mode, text around point etc. to guess Math content to read."
  (declare (special emacspeak-maths))
  (setf(emacspeak-maths-input emacspeak-maths)
       (cond
        ((and (memq major-mode '(tex-mode plain-tex-mode latex-mode ams-tex-mode))
              (featurep 'texmathp))
         (emacspeak-maths-guess-tex))
        ((and (eq major-mode 'eww-mode)
              (not
               (string-equal
                (get-text-property (point) 'shr-alt)
                "No image under point")))
         (get-text-property (point) 'shr-alt))
        (mark-active
         (buffer-substring (region-beginning)(region-end))))))

(defun emacspeak-maths-enter (latex)
  "Send a LaTeX expression to Maths server.
Tries to guess default based on context."
  (interactive
   (list
    (progn (emacspeak-maths-guess-input) ;guess based on context
           (read-from-minibuffer "LaTeX: "
                                 nil nil nil nil
                                 (emacspeak-maths-input emacspeak-maths)))))
  (declare (special emacspeak-maths))
  (emacspeak-maths-ensure-server)
  (setf (emacspeak-maths-input emacspeak-maths) latex)
  (process-send-string
   (emacspeak-maths-client-process emacspeak-maths)
   (format "enter: %s"latex)))

(cl-loop
 for move in
 '("left" "right" "up" "down" "root" "depth")
 do
 (eval
  `(defun ,(intern (format "emacspeak-maths-%s" move)) ()
     ,(format "Move %s in current Math expression." move)
     (interactive)
     (declare (special emacspeak-maths))
     (process-send-string
      (emacspeak-maths-client-process emacspeak-maths)
      ,(format "%s:\n" move)))))

;;}}}
;;{{{ Speaking Output:

;;}}}
;;{{{ Displaying Output:

;;}}}
;;{{{ Output: spoken-math mode:

(define-derived-mode emacspeak-maths-spoken-mode special-mode
  "Spoken Math On The Complete Audio Desktop"
  "Special mode for interacting with Spoken Math.

This mode is used by the special buffer that displays spoken math
returned from the Node server.
This mode is similar to Emacs' `view-mode'.
see the key-binding list at the end of this description.
Emacs online help facility to look up help on these commands.

\\{emacspeak-maths-spoken-mode-map}"
  (goto-char (point-min))
  (setq header-line-format "Spoken Math")
  (modify-syntax-entry 10 ">"))

(defun emacspeak-maths-setup-output ()
  "Set up output buffer for displaying spoken math."
  (with-current-buffer (get-buffer-create "*Spoken Math*")
    (let ((inhibit-read-only t))
      (erase-buffer))
    (emacspeak-maths-spoken-mode)
    (current-buffer)))

;;}}}
;;{{{ Helpers:

(defun emacspeak-maths-speak-alt ()
  "Speak alt text as Maths.
For use on Wikipedia pages  for example."
  (interactive)
  (cl-assert (eq major-mode 'eww-mode) "Not in an EWW buffer.")
  (let ((alt-text (get-text-property (point) 'shr-alt)))
    (unless (string-equal alt-text "No image under point")
      (funcall-interactively #'emacspeak-maths-enter alt-text))))

;;}}}
;;{{{ Muggle: Speak And Browse Math
(when (featurep 'hydra)
  (global-set-key
   (kbd "s-SPC")
   (defhydra emacspeak-maths-navigator
     (:body-pre (emacspeak-muggles-body-pre "Spoken Math")
                :pre emacspeak-muggles-pre
                :post emacspeak-muggles-post)
     "Spoken Math"
     ("SPC" emacspeak-maths-enter "enter")
     ("a" emacspeak-maths-speak-alt "Alt Text")
     ("d" emacspeak-maths-depth "Depth")
     ("r" emacspeak-maths-root "Root")
     ("<up>" emacspeak-maths-up "Up")
     ("<down>" emacspeak-maths-down"down")
     ("<left>" emacspeak-maths-left "left")
     ("<right>" emacspeak-maths-right "right"))))

;;}}}
(provide 'emacspeak-maths)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
