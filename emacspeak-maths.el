;;; emacspeak-maths.el --- Speak Mathematics 
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

(require 'cl-lib)
(declaim  (optimize  (safety 0) (speed 3)))
;(require 'emacspeak-preamble)



(require 'comint)
(require 'derived)

;;}}}
;;{{{ Customizations And Variables:

(defgroup emacspeak-maths nil
  "Customize Emacspeak  Maths.")

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

(defun emacspeak-maths-comint-filter (output)
  "Process output filter."
  (declare (special emacspeak-maths))
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
        output))))

;;}}}
;;{{{ Setup:

(defun emacspeak-maths-start-node ()
  "Start up Node as a comint sub-process."
  (declare (special emacspeak-maths-inferior-program emacspeak-maths))
  (let ((comint (make-comint emacspeak-maths-inferior-program)))
    (setf emacspeak-maths
          (make-emacspeak-maths
           :buffer comint
           :process (get-buffer-process buffer)))
    (with-current-buffer comint
      (add-hook 'comint-preoutput-filter-functions #'emacspeak-maths-comint-filter))))

;;}}}
(provide 'emacspeak-maths)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
