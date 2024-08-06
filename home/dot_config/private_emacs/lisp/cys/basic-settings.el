;;; basic-settings -- my config for basic Emacs packages.

;; My configurations for basic Emacs packages, i.e. packages so basic that are
;; not considered packages.

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

;;; basic

;;;; load files
(require 'cys/common-utils)

;;;; kernel name
(defvar kernel-name (string-trim (shell-command-to-string "uname -s")))

;;;; disable warnings
(setq warning-suppress-types '((comp)))

;;;; ring bell.
(setq ring-bell-function 'ignore)

;;;; encoding
(prefer-coding-system 'utf-8-unix)

;;;; enable case-insensitive search globally
(setq-default case-fold-search t)
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

;;;; close all files when closing a dedicated frame.
(add-to-list
 'delete-frame-functions
 (lambda (frame)
   (when (eql 2 (length (frame-list)))
     (setq case-fold-search nil)
     (dolist (buffer (buffer-list))
       (let ((name (buffer-name buffer)))
         (unless (string-match
                  "^\\*\\(scratch\\|Minibuf-[0-9]+\\|GNU Emacs\\|Messages\\|code-conversion-work\\|Echo Area [0-9]+\\|eldoc for .*\\)\\*$"
                  name)
           (kill-buffer name)))))))

;;;; backups and auto-saves
;; Save #*# files to $XDG_CONFIG_HOME/emacs/auto-saves.
(let ((auto-save-dir "~/.config/emacs/auto-saves"))
  (make-directory auto-save-dir 'parents)
  (setq auto-save-file-name-transforms
        `((".*" ,auto-save-dir t))))
;; Save *~ files to $XDG_CONFIG_HOME/emacs/backups.
(let ((backup-dir "~/.config/emacs/backups"))
  (make-directory backup-dir 'parents)
  (setq backup-directory-alist
        `(("." . ,backup-dir))))

;;;; use a space to separate words of different origins
(setq fill-separate-heterogeneous-words-with-space t)

;;;; handle text/template
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" nil t))

;;; visual

;;;; tab settings
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;;; display fill column indicator
(let ((callback (lambda ()
                  (set-fill-column 79)
                  (display-fill-column-indicator-mode 1))))
  (add-hook 'text-mode-hook callback)
  (add-hook 'prog-mode-hook callback))

;;;; font settings
(dolist (font-pair '(("Segoe UI Emoji" . 1.0)
                     ("Noto Color Emoji" . 0.95)
                     ("微软雅黑" . 1.1)
                     ("Source Han Sans CN" . 1.2)
                     ("Source Han Mono SC" . 1.2)))
  (add-to-list 'face-font-rescale-alist font-pair))

(when (string-prefix-p "MSYS" kernel-name)
  (set-fontset-font t 'unicode "Consolas")
  (set-fontset-font t 'emoji "Segoe UI Emoji")
  (set-fontset-font t 'han "微软雅黑"))

;;;; faces
(set-face-background 'default "black")
(set-face-foreground 'default "white")
(set-face-attribute
 'default nil
 :height (cond
          ((string-prefix-p "MSYS" kernel-name) 100)
          ((equal (getenv "DMI_PRODUCT_NAME") "HP EliteBook 755 G5") 96)
          ((equal kernel-name "Darwin") 130)
          (t 100)))

;;;; disable tool-bar-mode
(tool-bar-mode -1)

;;;; initial frame size
(cys/alist-set default-frame-alist 'width 75)
(cys/alist-set default-frame-alist 'height 25)

;;;; DPI
(dolist (display (x-display-list))
  (let ((dpi (string-to-number (getenv "DPI")))
        (pixel-width (display-pixel-width display))
        (pixel-height (display-pixel-height display))
        (mm-width nil)
        (mm-height nil))
    (setq mm-width (round (* (/ pixel-width dpi) 25.4))
          mm-height (round (* (/ pixel-height dpi) 25.4)))
    (cys/alist-set display-mm-dimensions-alist display
                   (cons mm-width mm-height))))

;;; control

;;;; no delay for echoing keystrokes
(setq echo-keystrokes 0.01)

;;;; disable suggest-key-bindings
;;`execute-extended-command' is quite slow, so we have to disable it.
(setq suggest-key-bindings nil)

;;;; bind key for re-enabling font-lock-mode, since it can be quite slow
(defun font-lock-mode-restart ()
  (interactive)
  (font-lock-mode 0)
  (font-lock-mode 1))
(keymap-global-set "C-x r C-f C-l" 'font-lock-mode-restart)

;;;; man window switching method
(setq Man-notify-method 'thrifty)

;;;; disable s-q on macOS for quitting (whoever added that binding was ******)
(when (equal kernel-name "Darwin")
  (keymap-global-set "s-q" nil)
  (keymap-global-set "s-w" nil))

;;; finish up
(provide 'cys/basic-settings)

;;; basic-settings.el ends here
