;;; vs-emulation-mode.el --- Certain proprietary IDE emulation mode -*- lexical-binding: t -*-

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
;;   * Neither the name of the copyright holder(s) nor the names of any
;;     contributors may be used to endorse or promote products derived from
;;     this software without specific prior written permission.
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
;; * sending your data to the NSA
;; * and last but not least, random crashes

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

(defvar vs-emulation--timer-list nil)
(defvar vs-emulation--memory-hog nil)

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
  (message "Please wait while the VS emulation mode is being %s…"
           (if vs-emulation-mode "prepared" "shut down"))
  (dolist (timer vs-emulation--timer-list)
    (when timer
      (cancel-timer timer)))
  (setq vs-emulation--timer-list nil)
  (setq vs-emulation--memory-hog nil)
  (if vs-emulation-mode
      (progn
        (setq vs-emulation--timer-list
              (list (run-with-idle-timer 0 t 'vs-emulation--slow-down)
                    (run-with-timer 600 600 'vs-emulation--freeze)
                    (run-with-timer 1800 1800 'vs-emulation--crash)))
        (setq vs-emulation--memory-hog (number-sequence 1 100000000)))
    ;; Turning the mode off is too fast without this sleep.
    (sleep-for (+ 5.0 (cl-random 5.0)))))

;;;###autoload
(put 'vs-emulation-mode 'disabled
     "This mode is a joke. It can and eventually WILL crash Emacs.")

;;; vs-emulation-mode.el ends here
