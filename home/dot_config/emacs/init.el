;;; init.el --- my emacs config file.

;;; License:

;; Copyright (C) 2024  Yushun Cheng <chengys@disroot.org>
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

;;; Prelude:

;;;; Basic settings.
(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(defvar kernel-name (shell-command-to-string "uname -s"))

;;;; Add package-custom.
(require 'package-custom)

;;;; Bootstrap use-package.
(package-install-smart 'use-package)

;;;; chezmoi
(use-package chezmoi)

;;;; f
(use-package f)

;;;; package.
(use-package package
  :after (chezmoi f)
  :config
  (setq package-archives
        (if (equal (f-read-text "~/.config/synloc/country") "中国")
            '(("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
              ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
          '(("gnu" . "https://elpa.gnu.org/packages/")
            ("melpa" . "https://melpa.org/packages/")))))

;;;; use-package.
(use-package use-package
  :config
  (defun use-package-ensure-smart (name args _state &optional _no-refresh)
    "Function for use-package :ensure keyword."
    (dolist (ensure args)
      (let ((package
             (or (and (eq ensure t) (use-package-as-symbol name))
                 ensure)))
        (when (consp package)
          (use-package-pin-package (car package) (cdr-package))
          (setq package (car package)))
        (package-install-smart package))))
  (setq use-package-ensure-function 'use-package-ensure-smart)
  (setq use-package-always-ensure t))
 
;;; Packages:

;;;; adoc-mode
(use-package adoc-mode)

;;;; agda-mode
(when (executable-find "agda-mode")
  (load-file
   (let ((coding-system-for-read 'utf-8))
     (shell-command-to-string "agda-mode locate"))))

;;;; ahk-mode.
(when (string-prefix-p "MSYS" kernel-name)
  (use-package ahk-mode))

;;;; asm-mode
(use-package asm-mode
  :after (simple)
  :hook (asm-mode . (lambda () (indent-tabs-mode 1))))

;;;; AUCTeX.
(use-package tex
  :after (outline)
  :ensure auctex
  :hook ((latex-mode LaTeX-mode tex-mode TeX-mode) .
         (lambda ()
           (outline-minor-mode 1)
           (TeX-engine-set 'xetex))))

;;;; bibtex
(use-package bibtex)

;;;; ccls.
(use-package ccls
  :after (cc-mode evil-collection)
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (lsp) (hs-minor-mode 1)))
  :config
  (evil-collection-define-key 'motion 'c-mode-map
    (kbd "==") 'lsp-format-buffer)
  (evil-collection-define-key 'motion 'c++-mode-map
    (kbd "==") 'lsp-format-buffer))

;;;; cc-mode.
(use-package cc-mode
  :hook ((c-mode) . (lambda () (c-set-style "linux"))))

;;;; color-theme-tomorrow.
(use-package color-theme-tomorrow
  :config
  (color-theme-tomorrow--define-theme night-bright)
  (enable-theme 'tomorrow-night-bright))

;;;; company
(use-package company
  :demand t
  :bind (:map
         company-active-map
         ;; my binds
         ("TAB"           . company-select-next)
         ("<tab>"         . company-select-next)
         ("<backtab>"     . company-select-previous)
         ("C-h"           . company-complete-selection)
         ("C-<backspace>" . company-complete-selection)
         ("RET"           . nil)
         ("<return>"      . nil)
         ("<right>"       . company-complete-common))
  :config
  (setq company-idle-delay 0.05)
  (unless (string-prefix-p "MSYS" kernel-name)
    (global-company-mode)))

;;;; conf-mode.
(use-package conf-mode
  :mode ("\\.\\(service\\|timer\\|path\\)\\'" . conf-unix-mode))

;;;; c-source-config.
(use-package c-source-config)

;;;; dired.
(use-package dired)

;;;; display-fill-column-indicator.
(use-package display-fill-column-indicator
  :after (text-mode prog-mode simple)
  :hook ((text-mode prog-mode) .
         (lambda ()
           (set-fill-column 79)
           (display-fill-column-indicator-mode 1))))

;;;; elisp-mode
(use-package elisp-mode
  :hook ((emacs-lisp-mode) .
         (lambda ()
           (outline-minor-mode 1)
           (indent-tabs-mode -1))))

;;;; evil
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-tree)
  :after (undo-tree menu-bar simple)
  :demand t
  :bind (:map
         evil-motion-state-map
         ;; next/previous visual line.
         ("j" . evil-next-visual-line)
         ("<down>" . evil-next-visual-line)
         ("k" . evil-previous-visual-line)
         ("<up>" . evil-previous-visual-line)
         ;; next/previous line.
         ("j" . evil-next-line)
         ("<down>" . evil-next-line)
         ("k" . evil-previous-line)
         ("<up>" . evil-previous-line)
         ;;
         ("gj" . evil-next-visual-line)
         ("g <down>" . evil-next-visual-line)
         ("gk" . evil-previous-visual-line)
         ("g <up>" . evil-previous-visual-line)
         ;; buffers.
         ("<SPC>" . nil) ; Unset <SPC>.
         ("<SPC> k" . kill-buffer)
         ("<SPC> b" . switch-to-buffer)
         ("<SPC> <left>" . previous-buffer)
         ("<SPC> <right>" . next-buffer)
         ("<SPC> f" . find-file)
         ("<SPC> s" . save-buffer)
         ("<SPC> c" . save-buffers-kill-terminal)
         ;; windows.
         ("<SPC> 0" . delete-window)
         ("<SPC> 1" . delete-other-windows)
         ("<SPC> 2" . split-window-below)
         ("<SPC> 3" . split-window-right)
         ("<SPC> r" . window-swap-states)
         ("<SPC> o" . other-window)
         ;; Word counting.
         ("<SPC> w" . count-words)
         ;; Comment line.
         ("gc" . comment-line)
         ;; Tab and jumping.
         ("C-f" . nil)
         ("C-f" . evil-jump-foward)
         ;; Toggle case sensitivity.
         ("U" . toggle-case-fold-search)
         :map
         evil-insert-state-map
         ;; Auto comment.
         ("<return>" . comment-indent-new-line))
  :config
  (evil-define-command evil-quit (&optional force)
    "Kill the current buffer, and close the current window, current
frame, current terminal."
    :repeat nil
    (interactive "<!>")

    (let ((initial-window (get-buffer-window)))
      (kill-buffer)
      (condition-case nil
          (when (eq initial-window (get-buffer-window))
            (delete-window))
        (error
         (condition-case nil
             (delete-frame)
           (error
            (if force
               (save-buffers-kill-terminal)
             (save-buffers-kill-terminal))))))))
  (evil-mode 1))

;;;; evil-collection
(use-package evil-collection
  :after (evil man dired help info view)
  :config
  ;; Initialize.
  (evil-collection-init)
  ;; man.
  (evil-collection-define-key 'normal 'Man-mode-map
    (kbd "SPC") nil
    (kbd "u") 'scroll-down-command
    (kbd "d") 'scroll-up-command)
  ;; dired.
  (evil-collection-define-key 'normal 'dired-mode-map
    (kbd "SPC") nil
    (kbd "h") 'dired-up-directory
    (kbd "l") 'dired-find-file)
  ;; help.
  (evil-collection-define-key 'normal 'help-mode-map
    (kbd "SPC") nil)
  ;; info.
  (evil-collection-define-key 'normal 'Info-mode-map
    (kbd "SPC") nil)
  ;; view.
  (evil-collection-define-key 'normal 'view-mode-map
    (kbd "SPC") nil))

;;;; faces.
(use-package faces
  :config
  (set-face-background 'default "black")
  (set-face-foreground 'default "white")
  (set-face-attribute 'default nil
                      :height (if (string-prefix-p "MSYS" kernel-name)
                                  100
                                (if (equal (getenv "DMI_PRODUCT_NAME")
                                           "HP EliteBook 755 G5")
                                    96))))

;;;; files
(use-package files
  :config
  ;; Save #*# files to $XDG_CONFIG_HOME/emacs/auto-saves.
  (setq auto-save-file-name-transforms
        '((".*" "~/.config/emacs/auto-saves/" t)))
  ;; Save *~ files to $XDG_CONFIG_HOME/emacs/backups.
  (setq backup-directory-alist
        '(("." . "~/.config/emacs/backups")))
  ;; Handling templates.
  (add-to-list 'auto-mode-alist '("\\.tmpl\\'" nil t)))

;;;; fill
;; Apparently `fill' is not provided.
(setq fill-separate-heterogeneous-words-with-space t)

;;;; fish-mode.
(use-package fish-mode)

;;;; flycheck.
(use-package flycheck
  :after (evil-collection)
  :config
  (evil-collection-define-key 'motion 'flycheck-mode-map
    (kbd "]d") 'flycheck-next-error
    (kbd "[d") 'flycheck-previous-error)
  (setq flycheck-checker-error-threshold nil))

;;;; git-commit.
(use-package git-commit
  :hook (git-commit-mode . (lambda () (set-fill-column 69))))

;;;; go-mode
(use-package go-mode)

;;;; haskell-indentation
(use-package haskell-mode
  :after (evil)
  :config
  ;; Credit: https://www.reddit.com/r/spacemacs/comments/as8zkv/haskell_mode_indents_incorrectly/
  ;;
  ;; (defun haskell-indentation-open-above ()
  ;;   (interactive)
  ;;   (evil-beginning-of-line)
  ;;   (haskell-indentation-newline-and-indent)
  ;;   (evil-previous-line)
  ;;   (haskell-indentation-indent-line)
  ;;   (evil-append-line 1))
  (defun haskell-indentation-open-below ()
    (interactive)
    (evil-append-line 1)
    (haskell-indentation-newline-and-indent))
  (evil-define-key 'insert haskell-indentation-mode-map
    (kbd "<return>") 'haskell-indentation-newline-and-indent)
  (evil-define-key 'normal haskell-indentation-mode-map
    "o" 'haskell-indentation-open-below))

;;;; help.
(use-package help)

;;;; hideshow.
(use-package hideshow
  :after (evil-collection)
  :config
  (defun hs-toggle-block ()
    "Toggle hiding or showing the current block."
    (interactive)
    (if (invisible-p (point-at-eol))
        (hs-show-block)
      (hs-hide-block)))
  (evil-collection-define-key 'motion 'hs-minor-mode-map
    (kbd "<tab>") nil
    (kbd "<tab>") 'hs-toggle-block
    (kbd "TAB") nil
    (kbd "TAB") 'hs-toggle-block
    (kbd "<backtab>") nil
    (kbd "<backtab>") 'hs-hide-level))

;;;; info.
(use-package info)

;;;; input-switch.
(use-package input-switch
  :after (evil))

;;;; ivy
(use-package ivy
  :demand t
  :bind (:map
         ivy-minibuffer-map
         ;; My bindings.
         ("TAB"           . ivy-next-line)
         ("<tab>"         . ivy-next-line)
         ("<backtab>"     . ivy-previous-line)
         ("C-h"           . ivy-done)
         ("C-<backspace>" . ivy-done)
         ("RET"           . ivy-immediate-done)
         ("<return>"      . ivy-immediate-done)
         ("<right>"       . ivy-partial-or-done))
  :config
  (ivy-mode 1))

;;;; js.
(use-package js
  :hook (js-mode . (lambda () (indent-tabs-mode -1)))
  :config
  (setq js-indent-level 4))

;;;; lsp-haskell
(use-package lsp-haskell
  :after (lsp-mode))

;;;; lsp-mode
(use-package lsp-mode
  :after (evil)
  :demand t
  :config
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-disabled-clients '(pylsp ruff-lsp))
  :bind (:map
         evil-motion-state-map
         ;; workspaces
         ("C-c C-l wD" . lsp-disconnect)
         ("C-c C-l wd" . lsp-describe-session)
         ("C-c C-l ws" . lsp)
         ;; folders
         ("C-c C-l fa" . lsp-workspace-folders-add)
         ("C-c C-l fb" . lsp-workspace-blacklist-remove)
         ("C-c C-l fr" . lsp-workspace-folders-remove)
         ;; goto
         ("gD" . nil)
         ("gD" . lsp-find-declaration)
         ("gd" . nil)
         ("gd" . lsp-find-definition)
         :map
         evil-normal-state-map
         ;; formatting
         ("=" . nil)
         ("=r" . lsp-format-region)))

;;;; lsp-pylsp
(use-package lsp-pylsp
  :after (lsp-mode)
  :demand t
  :config
  (setq lsp-pylsp-plugins-jedi-completion-enabled nil)
  (setq lsp-pylsp-plugins-jedi-definition-enabled nil)
  (setq lsp-pylsp-plugins-jedi-hover-enabled nil)
  (setq lsp-pylsp-plugins-jedi-references-enabled nil)
  (setq lsp-pylsp-plugins-mccabe-enabled nil)
  (setq lsp-pylsp-plugins-pyflakes-enabled nil)
  (setq lsp-pylsp-plugins-yapf-enabled t))

;;;; lsp-pyright
(use-package lsp-pyright
  :after (lsp-mode)
  :demand t)

;;;; lsp-ruff
(use-package lsp-ruff-lsp
  :after (lsp-mode)
  :demand t)

;;;; lsp-ui
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

;;;; lua-mode.
(use-package lua-mode
  :hook (lua-mode . (lambda () (indent-tabs-mode -1)))
  :config
  (setq lua-indent-level 4))

;;;; magit
(use-package magit
  :bind (:map
         magit-status-mode-map
         ("SPC" . nil))
  :config
  (when (string-prefix-p "MSYS" kernel-name)
    (setq magit-git-executable "C:\\msys64\\usr\\bin\\git.exe")))

;;;; man.
(use-package man)

;;;; markdown-mode
(use-package markdown-mode
  :after (files)
  :config
  (defun math-paragraph ()
    (let ((magic-str "[ \t]*\\(\\$\\$\\|\\\\\\\\\\).*\\|"))
      (setq-local paragraph-start (concat magic-str paragraph-start)
                  paragraph-separate (concat magic-str paragraph-separate))))
  (add-hook 'markdown-mode-hook 'math-paragraph))

;;;; menu-bar.
(use-package menu-bar)

;;;; message.
(unless (string-prefix-p "MSYS" kernel-name)
  (use-package message
    :after (smtpmail)
    :config
    (setq message-send-mail-function 'smtpmail-send-it)))

;;;; org
(use-package org
  :config
  (setq org-startup-indented t)
  (set-face-attribute 'outline-1 nil :height 150)
  (set-face-attribute 'outline-2 nil :height 140)
  (set-face-attribute 'outline-3 nil :height 130)
  (set-face-attribute 'outline-4 nil :height 120)
  (set-face-attribute 'outline-5 nil :height 110)
  (set-face-attribute 'outline-6 nil :height 100))

;;;; outline.
(use-package outline
  :after (evil-collection)
  :config
  ;; (defun outline-folded-p ()
  ;;   "Return non-nil if this line is the heading of a folded body."
  ;;   ;; The package `subr' apparently does not provide its feature.
  ;;   (invisible-p (point-at-eol)))
  (defun outline-toggle-subtree ()
    "Toggle whether or not to show the subtree."
    (interactive)
    (if (invisible-p (point-at-eol))
        (outline-show-subtree)
      (outline-hide-subtree)))
  ;; Define key.
  (evil-collection-define-key 'motion 'outline-minor-mode-map
    (kbd "<tab>") nil
    (kbd "<tab>") 'outline-toggle-subtree
    (kbd "C-i") 'outline-toggle-subtree
    (kbd "<backtab>") nil
    (kbd "<backtab>") 'outline-show-children))

;;;; php-mode.
(use-package php-mode)

;;;; prog-mode.
(use-package prog-mode)

;;;; python
(use-package python
  :after (hideshow)
  :hook (python-mode . (lambda () (hs-minor-mode 1))))

;;;; pyvenv
(use-package pyvenv)

;;;; rustic.
(use-package rustic
  :after (simple hideshow evil-collection)
  :demand t
  :hook ((rust-mode) .
         (lambda ()
           (hs-minor-mode 1)
           (indent-tabs-mode -1)
           (setq tab-width 4)))
  :bind (:map
         rustic-mode-map
         ("C-c b" . rustic-cargo-build)
         ("C-c c" . rustic-cargo-check)
         ("C-c t" . rustic-cargo-test)
         ("C-c r" . rustic-cargo-run))
  :config
  (evil-collection-define-key 'motion 'rustic-mode-map
    (kbd "==") 'lsp-format-buffer)
  (setq lsp-rust-analyzer-diagnostics-disabled ["inactive-code"]))

;;;; rustic-compile.
(use-package rustic-compile
  :after (color-theme-tomorrow)
  :config
  (setq rustic-ansi-faces ansi-color-names-vector))

;;;; sgml-mode.
(use-package sgml-mode
  :after (hideshow)
  :hook (html-mode . (lambda () (hs-minor-mode 1))))

;;;; sh-script.
(use-package sh-script
  :hook ((sh-mode) .
         (lambda ()
           (setq tab-width sh-basic-offset)
           (indent-tabs-mode -1)))
  :config
  (setq sh-basic-offset 2))

;;;; simple.
(use-package simple
  :config
  (setq-default indent-tabs-mode nil)
  ;; The function `execute-extended-command' is apparently slow.
  (setq suggest-key-bindings nil))

;;;; smtpmail.
(unless (string-prefix-p "MSYS" kernel-name)
  (use-package smtpmail
    :config
    (setq smtpmail-local-domain (system-name))))

;;;; term
(use-package term
  :after (evil-collection)
  :config
  (define-key term-mode-map (kbd "RET") nil)
  (define-key term-mode-map (kbd "C-j") nil)
  (define-key term-mode-map (kbd "M-x") nil)
  (evil-collection-define-key 'normal 'term-mode-map
    (kbd "RET") 'term-send-input))

;;;; text-mode
(use-package text-mode)

;;;; tide.
(use-package tide)

;;;; tool-bar.
(use-package tool-bar
  :config
  (tool-bar-mode -1))

;;;; typescript-mode.
(use-package typescript-mode
  :hook (typescript-mode . (lambda () (lsp) (hs-minor-mode 1))))

;;;; undo-tree
(use-package undo-tree
  :config
  (let ((dir (concat user-emacs-directory "undo-tree")))
    (make-directory dir t)
    (setq undo-tree-history-directory-alist
          `(("." . ,dir))))
  (global-undo-tree-mode))

;;;; view.
(use-package view
  :bind (:map
         view-mode-map
         ("SPC" . nil)))

;;;; yaml-mode.
(use-package yaml-mode)

;;;; yapfify
(use-package yapfify
  :after (evil-collection)
  :config
  (evil-collection-define-key 'motion 'python-mode-map
    (kbd "==") 'yapfify-buffer))

;;;; server
(use-package server
  :config (server-start))
