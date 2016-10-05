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


;;; Spoken mathematics on the emacspeak audio desktop.
;;; Use a NodeJS based speech-rule-engine for Mathematics as the backend
;;; for processing mathematical markup.
;;; The result of this processing is an annotated S-expression that is rendered via Emacspeak's speech  facilities.
;;; Annotations  follow Aural CSS as implemented in Emacspeak,
;;; This allows us to map these expressions to aural properties supported by specific TTS engines.


;;; Code:

;;}}}
;;{{{  Required modules

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
;(require 'emacspeak-preamble)



(require 'comint)
(require 'derived)

;;}}}
;;{{{ Customizations And Variables:

(defgroup emacspeak-maths nil
  "Customize Emacspeak  Maths."
  :group 'emacspeak)

(defcustom emacspeak-maths-inferior-program
  (or (executable-find "node")
      (expand-file-name "~/.nvm/versions/node/v6.4.0/bin/node"))
  "Location of `node' executable.
Default value uses the version of `node' set configured via NVM."
  :type 'string
  :group 'emacspeak-maths)


(cl-defstruct emacspeak-maths
  buffer ; comint buffer 
  process ; node process handle 
output ; where output is displayed
counter ; request counter
results ; s-expression  received from node
)

(defvar emacspeak-maths nil
  "Structure holding all runtime context.")

;;}}}
;;{{{ Process Filter:

(defun emacspeak-maths-parse-output (output &optional acc)
  (when (and output(stringp output))
    (let* ((start (string-match "BEGINOUTPUT[0-9]+: .* :ENDOUTPUT" output)))
    (if start
        (let* ((rest (cl-subseq output (+ start 11)))
               (colon (string-match ": " rest))
               (number (cl-subseq rest 0 colon))
               (rest (cl-subseq rest (+ colon 2)))
               (end (string-match " :ENDOUTPUT" rest))
               (result (cl-subseq rest 0 end)))
          ;;(push (cons (car (read-from-string number)) result) ems-results)
          (and output
               (emacspeak-maths-parse-output (cl-subseq rest end)
                                        (cons (cons (car (read-from-string number)) result) acc))))
      acc))))

(defun emacspeak-maths-comint-filter (output)
  "Process output filter."
  (declare (special emacspeak-maths))
  (when (and output (stringp output))
    (with-current-buffer (get-buffer (emacspeak-maths-output emacspeak-maths))
    (let ((result (emacspeak-maths-parse-output output)))
      (when result 
        (setf (emacspeak-maths-results emacspeak-maths)
              (append result (emacspeak-maths-results emacspeak-maths)))
        (mapc
         #'(lambda (x)
             (insert (cdr x))
             (insert "\n"))
         result)
        output)))))

;;}}}
;;{{{ Setup:
(defvar emacspeak-maths--init
  (concat
    "var mjx = require('mathjax-node');"
    "var sre = require('speech-rule-engine');"
    "sre.setupEngine({markup: 'acss'});"
    "var runWithCounter = function(counter, callback, args) {"
    "  var result = callback.apply(sre, args);"
    "  console.log('BEGINOUTPUT' + counter + ': ' + result + ' :ENDOUTPUT');"
    "};\n")
  "Initialization code we send the node process on startup.")

(defun emacspeak-maths-start ()
  "Start up Node as a comint sub-process."
  (declare (special emacspeak-maths-inferior-program emacspeak-maths
                    emacspeak-maths--init))
  (let ((comint (make-comint "Maths" emacspeak-maths-inferior-program)))
    (setf emacspeak-maths
          (make-emacspeak-maths
           :output (get-buffer-create "*Spoken Math*")
           :buffer comint
           :process (get-buffer-process comint)))
    (with-current-buffer comint
      (add-hook 'comint-preoutput-filter-functions #'emacspeak-maths-comint-filter)
      )
   (process-send-string
   (emacspeak-maths-process emacspeak-maths)
   emacspeak-maths--init)))

;;}}}
(provide 'emacspeak-maths)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}

