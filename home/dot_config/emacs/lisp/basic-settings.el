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

;;; Code:

;;;; Get kernel name.
(defvar kernel-name (shell-command-to-string "uname -s"))

;;;; Disable warnings
(setq warning-suppress-types '((comp)))

;;;; Disable ring bell.
(setq ring-bell-function 'ignore)

;;;; No delay for echoing keystrokes.
(setq echo-keystrokes 0.01)

;;;; Encoding.
(prefer-coding-system 'utf-8-unix)

;;;; Disable suggest-key-bindings
(setq suggest-key-bindings nil)
;; Disable it, since `execute-extended-command' is quite slow.

;;;; Tab settings.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;; Display fill column indicator.
(let ((callback (lambda ()
                  (set-fill-column 79)
                  (display-fill-column-indicator-mode 1))))
  (add-hook 'text-mode-hook callback)
  (add-hook 'prog-mode-hook callback))

;;;; Font settings.
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

;;;; Faces.
(set-face-background 'default "black")
(set-face-foreground 'default "white")
(set-face-attribute 'default nil
                    :height (if (string-prefix-p "MSYS" kernel-name) 100
                              (if (equal (getenv "DMI_PRODUCT_NAME")
                                         "HP EliteBook 755 G5")
                                  96)))

;;;; Disable tool-bar-mode
(tool-bar-mode -1)

;;;; Enable case-insensitive search globally.
(setq-default case-fold-search t)
(setq completion-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

;;;; Close all files when closing a dedicated frame.
(add-to-list
 'delete-frame-functions
 (lambda (frame)
   (when (eq 2 (length (frame-list)))
     (setq case-fold-search nil)
     (dolist (buffer (buffer-list))
       (let ((name (buffer-name buffer)))
         (unless (string-match
                  "^\\*\\(scratch\\|Minibuf-[0-9]+\\|GNU Emacs\\|Messages\\|code-conversion-work\\|Echo Area [0-9]+\\|eldoc for .*\\)\\*$"
                  name)
           (kill-buffer name)))))))

;;;; Backups and auto-saves.
;; Save #*# files to $XDG_CONFIG_HOME/emacs/auto-saves.
(setq auto-save-file-name-transforms
      '((".*" "~/.config/emacs/auto-saves/" t)))
;; Save *~ files to $XDG_CONFIG_HOME/emacs/backups.
(setq backup-directory-alist
      '(("." . "~/.config/emacs/backups")))

;;;; Handle text/template.
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" nil t))

;;;; Use a space to separate words of different origins.
(setq fill-separate-heterogeneous-words-with-space t)
