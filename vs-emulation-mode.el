;;; vs-emulation-mode.el --- Emulate a certain proprietary IDE -*- lexical-binding: t -*-

;; Author: Fanael Linithien <fanael4@gmail.com>
;; URL: https://github.com/Fanael/vs-emulation-mode
;; Version: 0.1
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2014, Fanael Linithien
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; Minor mode providing a faithful emulation of a certain proprietary IDE.
;;
;; Currently implemented features:
;; * large memory footprint
;; * random slowdowns
;; * random freezes
;; * random crashes
;; * sending your data to the NSA (may be buggy)
;; * displaying worthless error dialogs

;;; Code:

(require 'cl-lib)

(defun vs-emulation--slow-down ()
  "Randomly slow down Emacs by sleeping."
  (when (= 0 (cl-random 20))
    (sleep-for (+ 0.2 (cl-random 0.3)))))

(defun vs-emulation--freeze ()
  "Randomly freeze Emacs for a while.

All live buffers are sent to the NSA."
  (message "Please wait while we send all the live buffers to the NSA…")
  (when (= 0 (cl-random 3))
    (dotimes (_ 200000000))))

(defun vs-emulation--crash ()
  "Randomly attempt to crash Emacs."
  (when (= 0 (cl-random 4))
    (funcall (make-byte-code nil (string-make-unibyte (make-string 100000 #o300)) [] 0))))

(defconst vs-emulation--error-list
  ["Emacs has encountered an unexpected error."
   "Emacs is waiting for an operation to complete."
   "Option is not currently valid."
   "Out of memory."
   "Overflow."
   "Division by zero."
   "Unexpected Error: null"]
  "Vector of worthless error messages.")

(defun vs-emulation--dialog ()
  "Randomly display worthless error dialogs."
  (let ((last-nonmenu-event nil)
        (use-dialog-box t)
        (message (aref vs-emulation--error-list
                       (cl-random (length vs-emulation--error-list)))))
    (while (> 6 (cl-random 10))
      (ding)
      (x-popup-dialog (selected-frame)
                      `(,message ("Ok" . t) ("Cancel" . t)) "Error"))))

(defvar vs-emulation--timer-list nil)
(defvar vs-emulation--memory-hog nil)

(defun vs-emulation--mode* (modeon)
  "Turn the VS emulation mode on or off.
If MODEON is non-nil, turn the mode on, otherwise turn it off."
  (message "Please wait while the VS emulation mode is being %s…"
           (if modeon "prepared" "shut down"))
  (dolist (timer vs-emulation--timer-list)
    (when timer
      (cancel-timer timer)))
  (setq vs-emulation--timer-list nil)
  (setq vs-emulation--memory-hog nil)
  (if modeon
      (progn
        (setq vs-emulation--timer-list
              (list (run-with-idle-timer 0 t #'vs-emulation--slow-down)
                    (run-with-timer 240 240 #'vs-emulation--dialog)
                    (run-with-timer 600 600 #'vs-emulation--freeze)
                    (run-with-timer 1800 1800 #'vs-emulation--crash)))
        (setq vs-emulation--memory-hog (number-sequence 1 100000000)))
    ;; Turning the mode off is too fast without this sleep.
    (sleep-for (+ 5.0 (cl-random 5.0)))))

;;;###autoload
(define-minor-mode vs-emulation-mode
  "Faithful emulation of a certain proprietary IDE.

Toggle Vs-Emulation mode on or off. With a prefix argument ARG, enable
Vs-Emulation mode if ARG is positive, and disable it otherwise. If called from
Lisp, enable the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'."
  :init-value nil
  :lighter " VS"
  :keymap nil
  :global t
  ;; Do the work inside an idle timer so it can't be easily aborted with C-g.
  (run-with-idle-timer 0 nil #'vs-emulation--mode* vs-emulation-mode))

;;;###autoload
(put #'vs-emulation-mode 'disabled
     "This mode is a joke. It can and eventually WILL crash Emacs.")

(provide 'vs-emulation-mode)
;;; vs-emulation-mode.el ends here
