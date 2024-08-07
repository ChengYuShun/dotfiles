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

;;; prelude

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
(setq straight-allow-recipe-inheritance t)
(setq straight-use-package-by-default t)

;;;; Load basic-settings.el
(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(require 'cys/common-utils)
(require 'cys/basic-settings)

;;; packages with a custom recipe

;;;; org
(use-package org
  :after (evil)
  :commands (org-mode org-agenda)
  :straight (org :type git
                 :remote "origin"
                 :fork (:host github
                        :repo "ChengYuShun/org-mode"
                        :branch "cys"
                        :remote "github"))
  :bind (:map org-mode-map
         ("C-c l" . nil)
         ("C-c l y" . org-store-link)
         ("C-c l p" . org-insert-link)
         ("C-c l t" . org-toggle-link-display)
         ;; LaTeX preview
         ("C-c m t" . cys/org-toggle-latex-preview)
         ("C-c m s" . cys/org-show-latex-preview)
         ("C-c m h" . cys/org-hide-latex-preview)
         ;; inline images
         ("C-c i t" . org-toggle-inline-images)
         ("C-c i s" . org-display-inline-images)
         ("C-c i h" . org-remove-inline-images)
         ;; reserved for org-roam
         ("C-c n i g" . cys/org-roam-node-insert-global)
         ("C-c n i n" . cys/org-roam-node-insert-non-global)
         ("C-c n l y" . org-roam-link-store)
         ("C-c n l p" . org-roam-link-paste)
         ("C-c n l o" . org-roam-link-open)
         ("C-c n b" . org-roam-buffer-toggle)
         ("C-c n d" . cys/org-roam-node-delete)
         ("C-c n g" . cys/org-roam-global-toggle))
  :hook (org-mode . (lambda ()
                      (display-fill-column-indicator-mode -1)
                      (visual-line-mode 1)
                      (set-fill-column 70)
                      (org-fragtog-mode)))
  :config (require 'cys/org-config))

;;;; org-roam
(use-package org-roam
  :init
  (defvar cys/org-roam-repo (file-truename "~/org-roam")
    "The repository of Org-roam Zettels.")
  :straight (org-roam :type git
                      :host github
                      :repo "org-roam/org-roam"
                      :branch "main"
                      :remote "origin"
                      :fork (:host github
                             :repo "ChengYuShun/org-roam"
                             :branch "cys"
                             :remote "github"))
  :custom (org-roam-directory (concat cys/org-roam-repo "/zettels"))
  :commands (cys/org-roam-node-find-global)
  :config
  (require 'cys/org-roam-config))

;;;; yapfify
(use-package yapfify
  :straight (:host github :repo "ChengYuShun/yapfify.el" :branch "no_modify")
  :commands (yapfify-buffer))

;;; packages without custom recipe

;;;; adoc-mode
(use-package adoc-mode
  :commands (adoc-mode))

;;;; agda-mode
(when (or (executable-find "agda-mode")
          (and (executable-find "stack")
               (eql 0 (call-process "stack" nil nil nil
                                    "exec" "--" "which" "agda-mode"))))
  (load-file
   (let ((coding-system-for-read 'utf-8))
     (shell-command-to-string "agda-mode-wrapper locate"))))

;;;; ahk-mode.
(when (eq system-type 'windows-nt)
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
  (unless (eq system-type 'windows-nt)
    (global-company-mode)))

;;;; conf-mode
(use-package conf-mode
  :straight nil
  :mode ("\\.\\(service\\|timer\\|path\\)\\'" . conf-unix-mode))

;;;; elisp-mode
(use-package elisp-mode
  :straight nil
  :hook (emacs-lisp-mode . cys/emacs-lisp-mode-hook)
  :config
  (defun cys/emacs-lisp-mode-hook ()
    "Initialize Emacs Lisp mode."
    (outline-minor-mode 1)
    (setq-local tab-width 8)))

;;;; epresent
(use-package epresent
  :commands (epresent-run))

;;;; evil
(use-package evil

  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-tree)

  :demand t

  :bind (:map evil-motion-state-map
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
         ;; no return please
         ("RET" . nil)
         ;; unbind space
         ("SPC" . nil)
         ;; buffers
         ("SPC k" . kill-buffer)
         ("SPC b" . switch-to-buffer)
         ("SPC <left>" . previous-buffer)
         ("SPC <right>" . next-buffer)
         ("SPC f" . find-file)
         ("SPC s" . save-buffer)
         ("SPC c" . save-buffers-kill-terminal)
         ;; windows.
         ("SPC 0" . delete-window)
         ("SPC 1" . delete-other-windows)
         ("SPC 2" . split-window-below)
         ("SPC 3" . split-window-right)
         ("SPC r" . window-swap-states)
         ("SPC o" . other-window)
         ;; Word counting.
         ("SPC w" . count-words)
         ;; magit
         ("SPC g" . magit-status)
         ;; org-agenda
         ("SPC a" . org-agenda)
         ;; org-roam
         ("SPC n" . cys/org-roam-node-find-global)
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
         ("g d" . nil)
         ("g D" . nil)
         ;; Comment line.
         ("g c" . comment-line)
         ;; navigation bindings
         ("RET" . cys/evil-open-link)
         ("g u" . cys/evil-go-up)
         ("g j" . cys/evil-goto-next)
         ("g k" . cys/evil-goto-prev)
         ("g b" . cys/evil-go-back)

         :map evil-insert-state-map
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

  (defun cys/evil-go-up ()
    "Stub function, intended to be used to go back up."
    (interactive)
    nil)

  (defun cys/evil-goto-next ()
    "Stub function, intended to be used to go to the next page."
    (interactive)
    nil)

  (defun cys/evil-goto-prev ()
    "Stub function, intended to be used to go to the previous page."
    (interactive)
    nil)

  (defun cys/evil-open-link ()
    "Stub function, intended to be used to follow a link."
    (interactive)
    nil)

  (defun cys/evil-go-back ()
    "Stub function, intended to be used to go back to the previous page."
    (interactive)
    nil)

  (evil-mode 1))

;;;; evil-collection
(use-package evil-collection
  :after (evil)
  :demand t
  :config
  (message "evil-collection loaded.")
  ;; Hook for each mode.
  (defvar cys/evil-collection-mode-hooks (make-hash-table))
  ;; dired
  (puthash 'dired
           (lambda ()
             (evil-define-key* 'normal dired-mode-map
               (kbd "SPC") nil
               (kbd "h") 'dired-up-directory
               (kbd "l") 'dired-find-file))
           cys/evil-collection-mode-hooks)
  ;; flycheck
  (puthash 'flycheck
           (lambda ()
             (evil-define-key* 'motion flycheck-mode-map
               (kbd "]d") 'flycheck-next-error
               (kbd "[d") 'flycheck-previous-error))
           cys/evil-collection-mode-hooks)
  ;; help
  (puthash 'help
           (lambda ()
             (evil-define-key 'normal help-mode-map
               (kbd "SPC") nil
               [remap cys/evil-go-back] 'help-go-back))
           cys/evil-collection-mode-hooks)
  ;; info
  (puthash 'info
           (lambda ()
             (evil-define-key 'normal Info-mode-map
               (kbd "SPC") nil
               [remap cys/evil-open-link] 'Info-follow-nearest-node
               [remap cys/evil-go-up] 'Info-up
               [remap cys/evil-goto-next] 'Info-next
               [remap cys/evil-goto-prev] 'Info-prev
               [remap cys/evil-go-back] 'Info-last))
           cys/evil-collection-mode-hooks)
  ;; man
  (puthash 'man
           (lambda () (evil-define-key* 'normal Man-mode-map
                        (kbd "SPC") nil
                        (kbd "u") 'scroll-down-command
                        (kbd "d") 'scroll-up-command))
           cys/evil-collection-mode-hooks)
  ;; outline
  (puthash 'outline
           (lambda ()
             (evil-define-key* 'motion outline-minor-mode-map
               (kbd "<tab>") nil
               (kbd "<tab>") 'outline-toggle-subtree
               (kbd "C-i") 'outline-toggle-subtree
               (kbd "<backtab>") nil
               (kbd "<backtab>") 'outline-show-children))
           cys/evil-collection-mode-hooks)
  ;; term
  (puthash 'term
           (lambda ()
             (define-key term-mode-map (kbd "RET") nil)
             (define-key term-mode-map (kbd "C-j") nil)
             (define-key term-mode-map (kbd "M-x") nil)
             (evil-define-key 'normal term-mode-map
               (kbd "RET") 'term-send-input))
           cys/evil-collection-mode-hooks)
  ;; view
  (puthash 'view
           (lambda () (evil-define-key 'normal view-mode-map (kbd "SPC") nil))
           cys/evil-collection-mode-hooks)
  ;; Add to `evil-collection-setup-hook'.
  (add-hook 'evil-collection-setup-hook
            (lambda (mode _mode-keymaps &rest _rest)
              (let ((callback (gethash mode cys/evil-collection-mode-hooks)))
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

;;;; htmlize
(use-package htmlize)

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
  (ivy-mode 1)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order))))

;;;; js
(use-package js
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
  :config
  (setq lua-indent-level 4))

;;;; magit
(use-package magit
  :commands (magit magit-status)
  :bind (:map
         magit-status-mode-map
         ("SPC" . nil))
  :hook (git-commit-mode . (lambda () (set-fill-column 69)))
  :config
  (when (eq system-type 'windows-nt)
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

;;;; org-fragtog
(use-package org-fragtog
  :commands (org-fragtog-mode))

;;;; org-roam-ui
(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

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
(use-package rustic
  :after (hideshow evil-collection)
  :demand t
  :hook ((rust-mode) .
         (lambda ()
           (hs-minor-mode 1)
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
  (setq lsp-rust-analyzer-diagnostics-disabled ["inactive-code"])
  (setq rustic-ansi-faces ansi-color-names-vector))

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
  :hook ((sh-mode) . (lambda () (setq tab-width sh-basic-offset)))
  :config
  (setq sh-basic-offset 2))

;;;; smtpmail.
;; (unless (eq system-type 'windows-nt)
;;   (use-package smtpmail
;;     :config
;;     (setq smtpmail-local-domain (system-name))))

;;;; swift-mode
(use-package swift-mode)

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
