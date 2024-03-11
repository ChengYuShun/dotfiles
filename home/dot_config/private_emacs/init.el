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

;;;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;; Install use-package.
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;;; Load basic-settings.el
(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(load "basic-settings")

;;; Packages:

;;;; adoc-mode
(use-package adoc-mode
  :commands (adoc-mode))

;;;; agda-mode
;; (when (executable-find "agda-mode")
;;   (load-file
;;    (let ((coding-system-for-read 'utf-8))
;;      (shell-command-to-string "agda-mode locate"))))

;;;; ahk-mode.
(when (string-prefix-p "MSYS" kernel-name)
  (use-package ahk-mode
    :commands (ahk-mode)))

;;;; asm-mode
(use-package asm-mode
  :hook (asm-mode . (lambda () (indent-tabs-mode 1))))

;;;; AUCTeX
(use-package auctex
  :hook ((LaTeX-mode) . (lambda () (outline-minor-mode 1)
                          (TeX-engine-set 'xetex))))

;;;; bibtex
(use-package bibtex
  :commands (bibtex-mode))

;;;; calendar
(setq calendar-week-start-day 1)

;;;; ccls.
;; (use-package ccls
;;   :after (cc-mode evil-collection)
;;   :hook ((c-mode c++-mode objc-mode cuda-mode) .
;;          (lambda () (lsp) (hs-minor-mode 1)))
;;   :config
;;   (evil-collection-define-key 'motion 'c-mode-map
;;     (kbd "==") 'lsp-format-buffer)
;;   (evil-collection-define-key 'motion 'c++-mode-map
;;     (kbd "==") 'lsp-format-buffer))

;;;; cc-mode.
(use-package cc-mode
  :straight nil
  :hook ((c-mode) . (lambda () (c-set-style "linux"))))

;;;; chezmoi
(use-package chezmoi
  :straight nil)

;;;; color-theme-tomorrow
(use-package color-theme-tomorrow
  :straight nil
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
         ("C-@"           . company-complete-selection)
         ("C-SPC"         . company-complete-selection)
         ("RET"           . nil)
         ("<return>"      . nil)
         ("<right>"       . company-complete-common))
  :config
  (setq company-idle-delay 0.05)
  (unless (string-prefix-p "MSYS" kernel-name)
    (global-company-mode)))

;;;; conf-mode
(use-package conf-mode
  :straight nil
  :mode ("\\.\\(service\\|timer\\|path\\)\\'" . conf-unix-mode))

;;;; elisp-mode
(use-package elisp-mode
  :straight nil
  :hook (emacs-lisp-mode
         . (lambda () (outline-minor-mode 1) (indent-tabs-mode -1))))

;;;; evil
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-tree)
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
         evil-normal-state-map
         ;; Formatting.
         ("=" . nil)
         ;; Goto definition/declaration.
         ("gd" . nil)
         ("gD" . nil)

         :map
         evil-insert-state-map
         ;; Auto comment.
         ("<return>" . comment-indent-new-line))
  :config
  (message "evil loaded.")
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
  :after (evil)
  :demand t
  :config
  (message "evil-collection loaded.")
  ;; Hook for each mode.
  (defvar evil-collection-mode-hooks (make-hash-table))
  ;; dired
  (puthash 'dired
           (lambda ()
             (evil-define-key* 'normal dired-mode-map
               (kbd "SPC") nil
               (kbd "h") 'dired-up-directory
               (kbd "l") 'dired-find-file))
           evil-collection-mode-hooks)
  ;; flycheck
  (puthash 'flycheck
           (lambda ()
             (evil-define-key* 'motion flycheck-mode-map
               (kbd "]d") 'flycheck-next-error
               (kbd "[d") 'flycheck-previous-error))
           evil-collection-mode-hooks)
  ;; help
  (puthash 'help
           (lambda () (evil-define-key 'normal help-mode-map (kbd "SPC") nil))
           evil-collection-mode-hooks)
  ;; info
  (puthash 'info
           (lambda () (evil-define-key 'normal Info-mode-map (kbd "SPC") nil))
           evil-collection-mode-hooks)
  ;; man
  (puthash 'man
           (lambda () (evil-define-key* 'normal Man-mode-map
                        (kbd "SPC") nil
                        (kbd "u") 'scroll-down-command
                        (kbd "d") 'scroll-up-command))
           evil-collection-mode-hooks)
  ;; outline
  (puthash 'outline
           (lambda ()
             (evil-define-key* 'motion outline-minor-mode-map
               (kbd "<tab>") nil
               (kbd "<tab>") 'outline-toggle-subtree
               (kbd "C-i") 'outline-toggle-subtree
               (kbd "<backtab>") nil
               (kbd "<backtab>") 'outline-show-children))
           evil-collection-mode-hooks)
  ;; term
  (puthash 'term
           (lambda ()
             (define-key term-mode-map (kbd "RET") nil)
             (define-key term-mode-map (kbd "C-j") nil)
             (define-key term-mode-map (kbd "M-x") nil)
             (evil-define-key 'normal term-mode-map
               (kbd "RET") 'term-send-input))
           evil-collection-mode-hooks)
  ;; view
  (puthash 'view
           (lambda () (evil-define-key 'normal view-mode-map (kbd "SPC") nil))
           evil-collection-mode-hooks)
  ;; Add to `evil-collection-setup-hook'.
  (add-hook 'evil-collection-setup-hook
            (lambda (mode _mode-keymaps &rest _rest)
              (let ((callback (gethash mode evil-collection-mode-hooks)))
                (when (functionp callback)
                  (eval `(,callback))))))
  ;; Initialize evil-collection.
  (evil-collection-init))

;;;; input-switch
(when (equal (getenv "XDG_CURRENT_DESKTOP") "KDE")
  (use-package input-switch
    :straight nil
    :after (evil)
    :config
    (input-switch-fcitx5)
    (add-hook 'evil-insert-state-entry-hook 'input-switch-enter)
    (add-hook 'evil-insert-state-exit-hook 'input-switch-exit)))

;;;; f
;; (use-package f)

;;;; fish-mode
(use-package fish-mode
  :commands (fish-mode))

;;;; flycheck
(use-package flycheck
  :commands (flycheck-mode global-flycheck-mode)
  :config
  (setq flycheck-checker-error-threshold nil))

;;;; go-mode
(use-package go-mode
  :commands (go-mode))

;;;; haskell-indentation
(use-package haskell-mode
  :after (evil)
  :commands (haskell-mode)
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

;;;; hideshow
(use-package hideshow
  :straight nil
  :after (evil)
  :commands (hs-minor-mode)
  :config
  (defun hs-toggle-block ()
    "Toggle hiding or showing the current block."
    (interactive)
    (if (invisible-p (point-at-eol))
        (hs-show-block)
      (hs-hide-block)))
  (evil-define-key* 'motion hs-minor-mode-map
    (kbd "<tab>") nil
    (kbd "<tab>") 'hs-toggle-block
    (kbd "TAB") nil
    (kbd "TAB") 'hs-toggle-block
    (kbd "<backtab>") nil
    (kbd "<backtab>") 'hs-hide-level))

;;;; ivy
(use-package ivy
  :demand t
  :bind (:map
         ivy-minibuffer-map
         ;; My bindings.
         ("TAB"           . ivy-next-line)
         ("<tab>"         . ivy-next-line)
         ("<backtab>"     . ivy-previous-line)
         ("C-@"           . ivy-done)
         ("C-SPC"         . ivy-done)
         ("RET"           . ivy-immediate-done)
         ("<return>"      . ivy-immediate-done)
         ("<right>"       . ivy-partial-or-done))
  :config
  (ivy-mode 1))

;;;; js
(use-package js
  :hook (js-mode . (lambda () (indent-tabs-mode -1)))
  :config
  (setq js-indent-level 4))

;;;; lsp-haskell
;; (use-package lsp-haskell
;;   :after (lsp-mode))

;;;; lsp-mode
(use-package lsp-mode
  :after (evil)
  :commands (lsp)
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
         ("=r" . lsp-format-region))
  :config
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-disabled-clients '(pylsp ruff-lsp)))

;;;; lsp-pylsp
;; (use-package lsp-pylsp
;;   :after (lsp-mode)
;;   :demand t
;;   :config
;;   (setq lsp-pylsp-plugins-jedi-completion-enabled nil)
;;   (setq lsp-pylsp-plugins-jedi-definition-enabled nil)
;;   (setq lsp-pylsp-plugins-jedi-hover-enabled nil)
;;   (setq lsp-pylsp-plugins-jedi-references-enabled nil)
;;   (setq lsp-pylsp-plugins-mccabe-enabled nil)
;;   (setq lsp-pylsp-plugins-pyflakes-enabled nil)
;;   (setq lsp-pylsp-plugins-yapf-enabled t))

;;;; lsp-pyright
(use-package lsp-pyright
  :after (lsp-mode python))

;;;; lsp-ruff
;; (use-package lsp-ruff-lsp
;;   :after (lsp-mode)
;;   :demand t)

;;;; lsp-ui
(use-package lsp-ui
  :commands (lsp-ui-mode)
  :hook (lsp-mode . lsp-ui-mode))

;;;; lua-mode.
(use-package lua-mode
  :hook (lua-mode . (lambda () (indent-tabs-mode -1)))
  :config
  (setq lua-indent-level 4))

;;;; magit
(use-package magit
  :commands (magit)
  :bind (:map
         magit-status-mode-map
         ("SPC" . nil))
  :hook (git-commit-mode . (lambda () (set-fill-column 69)))
  :config
  (when (string-prefix-p "MSYS" kernel-name)
    (setq magit-git-executable "C:\\msys64\\usr\\bin\\git.exe")))

;;;; markdown-mode
(use-package markdown-mode
  :commands (markdown markdown-mode)
  :config
  (defun math-paragraph ()
    (let ((magic-str "[ \t]*\\(\\$\\$\\|\\\\\\\\\\).*\\|"))
      (setq-local paragraph-start (concat magic-str paragraph-start)
                  paragraph-separate (concat magic-str paragraph-separate))))
  (add-hook 'markdown-mode-hook 'math-paragraph))

;;;; org
(use-package org
  :commands (org-mode)
  :bind (("C-x a" . org-agenda)
         :map org-mode-map
         ("C-c l" . org-store-link)
         ("C-c C-l" . org-insert-link))
  :hook (org-mode . (lambda ()
                      (display-fill-column-indicator-mode -1)
                      (visual-line-mode 1)
                      (set-fill-column 70)
                      (org-fragtog-mode)))
  :config
  (setq org-startup-indented t)
  (plist-put org-format-latex-options :scale 2)
  (setq org-deadline-warning-days 30)
  (setq org-agenda-files '("~/todos"))
  (setq org-agenda-start-on-weekday nil)
  (setq org-log-done t))

;;;; org-fragtog
(use-package org-fragtog
  :commands (org-fragtog-mode))

;;;; outline
(use-package outline
  :straight nil
  :commands (outline-mode outline-minor-mode)
  :config
  ;; (defun outline-folded-p ()
  ;;   "Return non-nil if this line is the heading of a folded body."
  ;;   ;; The package `subr' apparently does not provide its feature.
  ;;   (invisible-p (point-at-eol)))
  (message "outline loaded.")
  (defun outline-toggle-subtree ()
    "Toggle whether or not to show the subtree."
    (interactive)
    (if (invisible-p (point-at-eol))
        (outline-show-subtree)
      (outline-hide-subtree))))

;;;; php-mode
(use-package php-mode
  :commands (php-mode))

;;;; python
(use-package python
  :hook (python-mode . (lambda () (hs-minor-mode 1)))
  :after (evil)
  :config
  (evil-define-key 'motion python-mode-map "==" 'yapfify-buffer))

;;;; rustic.
;; (use-package rustic
;;   :after (simple hideshow evil-collection)
;;   :demand t
;;   :hook ((rust-mode) .
;;          (lambda ()
;;            (hs-minor-mode 1)
;;            (indent-tabs-mode -1)
;;            (setq tab-width 4)))
;;   :bind (:map
;;          rustic-mode-map
;;          ("C-c b" . rustic-cargo-build)
;;          ("C-c c" . rustic-cargo-check)
;;          ("C-c t" . rustic-cargo-test)
;;          ("C-c r" . rustic-cargo-run))
;;   :config
;;   (evil-collection-define-key 'motion 'rustic-mode-map
;;     (kbd "==") 'lsp-format-buffer)
;;   (setq lsp-rust-analyzer-diagnostics-disabled ["inactive-code"]))

;;;; rustic-compile.
;; (use-package rustic-compile
;;   :after (color-theme-tomorrow)
;;   :config
;;   (setq rustic-ansi-faces ansi-color-names-vector))

;;;; sgml-mode.
(use-package sgml-mode
  :hook (html-mode . (lambda () (hs-minor-mode 1))))

;;;; sh-script.
(use-package sh-script
  :hook ((sh-mode) .
         (lambda ()
           (setq tab-width sh-basic-offset)
           (indent-tabs-mode -1)))
  :config
  (setq sh-basic-offset 2))

;;;; smtpmail.
;; (unless (string-prefix-p "MSYS" kernel-name)
;;   (use-package smtpmail
;;     :config
;;     (setq smtpmail-local-domain (system-name))))

;;;; tide
;; (use-package tide)

;;;; typescript-mode.
;; (use-package typescript-mode
;;   :hook (typescript-mode . (lambda () (lsp) (hs-minor-mode 1))))

;;;; undo-tree
(use-package undo-tree
  :config
  (let ((dir (concat user-emacs-directory "undo-tree")))
    (make-directory dir t)
    (setq undo-tree-history-directory-alist
          `(("." . ,dir))))
  (global-undo-tree-mode))

;;;; yaml-mode.
(use-package yaml-mode
  :commands (yaml-mode))

;;;; yapfify
(use-package yapfify
  :straight (:host github :repo "ChengYuShun/yapfify.el" :branch "no_modify")
  :commands (yapfify-buffer))
