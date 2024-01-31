;;; chezmoi --- Functions for chezmoi.

;;; License:

;; Copyright (C) 2022  Yushun Cheng <chengys@disroot.org>
;;
;; This file is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Common functions for chezmoi.

;;; Code:

;;;; Load necessary packages.
(require 'window)
(require 'term)
(require 'dired)
(require 'diff)

;;;; Normal user.
(defvar chezmoi-dir "~/.local/share/chezmoi/home")
(defvar chezmoi-diff-buffer-name "*chezmoi diff*")
(defvar chezmoi-apply-buffer-name "*chezmoi apply*")
;;;###autoload
(defun chezmoi-cd ()
  "Open chezmoi directory in dired."
  (interactive)
  (dired chezmoi-dir))
;;;###autoload
(defun chezmoi-apply ()
  "Run 'chezmoi apply'."
  (interactive)
  (let ((term-buffer
	 (make-term chezmoi-apply-buffer-name "chezmoi" nil
		    "-D" "~" "-S" chezmoi-dir "apply" "--no-tty")))
    (switch-to-buffer term-buffer)
    (term-line-mode)))
;;;###autoload
(defun chezmoi-diff ()
  "Open 'chezmoi diff' in a new buffer with diff mode."
  (interactive)
  (switch-to-buffer chezmoi-diff-buffer-name)
  (erase-buffer)
  (call-process "chezmoi" nil chezmoi-diff-buffer-name nil
		"-D" "~/" "-S" chezmoi-dir "diff")
  (diff-mode))

;;;; Root user.
(defvar chezmoi-root-dir "~/.local/share/chezmoi/root")
(defvar chezmoi-root-diff-buffer-name "*schezmoi diff*")
(defvar chezmoi-root-apply-buffer-name "*schezmoi apply*")
;;;###autoload
(defun chezmoi-root-cd ()
  "Open chezmoi directory for root in dired."
  (interactive)
  (dired chezmoi-root-dir))
;;;###autoload
(defun chezmoi-root-apply ()
  "Run 'schezmoi apply'."
  (interactive)
  (let ((term-buffer
	 (make-term chezmoi-root-apply-buffer-name "chezmoi" nil
		    "-D" "/" "-S" chezmoi-root-dir "apply" "no-tty")))
    (switch-to-buffer term-buffer)
    (term-line-mode)))
;;;###autoload
(defun chezmoi-root-diff ()
  "Open 'schezmoi diff' in a buffer with diff mode."
  (interactive)
  (switch-to-buffer chezmoi-root-diff-buffer-name)
  (erase-buffer)
  (call-process "chezmoi" nil chezmoi-diff-buffer-name nil
		"-D" "/" "-S" chezmoi-root-dir "apply")
  (diff-mode))

(provide 'chezmoi)
;;; chezmoi.el ends here
