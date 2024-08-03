;;; org-config -- my config for org-mode.

;; My configurations for org-mode.

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

;;; Code:

;;;; load requirements
(require 'common-utils)

;;;; basic config
(setq org-startup-indented t)
(setq org-export-with-smart-quotes t)
(setq org-startup-folded 'nofold
      org-cycle-hide-drawer-startup t)
(evil-define-key 'normal org-mode-map
  [remap cys/evil-open-link] 'org-open-at-point
  [remap cys/evil-go-back] 'org-mark-ring-goto)

;;;; org-agenda
(setq org-deadline-warning-days 30)
(setq org-agenda-files '("~/org-agenda/"))
(setq org-agenda-start-on-weekday nil)
(setq org-log-done t)
(evil-define-key 'motion org-agenda-mode-map
  "q" 'org-agenda-quit
  "j" 'org-agenda-next-item
  "k" 'org-agenda-previous-item
  (kbd "RET") 'org-agenda-switch-to
  (kbd "<tab>") 'org-agenda-goto)
(evil-set-initial-state 'org-agenda-mode 'motion)

;;;; babel
(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t)
                               (python . t)))

;;;; org-roam
(cys/alist-set org-link-frame-setup 'file 'find-file-other-frame)
;; add custom key bindings for org-capture-mode (for whatever reason, it
;; appears that we cannot define key bindings for org-capture-mode by using
;; the :bind keyword provided by use-package)
(defun cys/org-capture-define-key ()
  (define-key org-capture-mode-map [remap evil-quit]
              'org-capture-kill)
  (define-key org-capture-mode-map [remap evil-save-modified-and-close]
              'org-capture-finalize)
  (remove-hook  'org-capture-mode-hook 'cys/org-capture-define-key))
(add-hook 'org-capture-mode-hook 'cys/org-capture-define-key)

;;;; latex preview
(setq org-startup-with-latex-preview t)
(setq org-latex-packages-alist '(("" "mhchem" t)
                                 ("" "tikz-cd" t)))

;;;;; SVG preview
(cys/alist-set
 org-preview-latex-process-alist
 'cys-svg
 '(:programs
   ("latex" "pdftocairo" "inkscape")
   :description "pdf > svg"
   :message "you need to install the programs: latex, pdftocairo and inkscape."
   :image-input-type "pdf" :image-output-type "svg"
   :image-size-adjust (1.7 . 1.5)
   :latex-compiler
   ("pdflatex -interaction nonstopmode -output-directory %o %f")
   :image-converter
   ("ltxpdf2svg %f %O %S '#ffffff'")))
(setq org-preview-latex-image-directory
      (concat (file-name-as-directory user-emacs-directory) "ltximg/"))
(setq org-preview-latex-default-process 'cys-svg)
(plist-put org-format-latex-options
           :scale (cond ((equal kernel-name "Darwin") 1.5)
                        (t 1)))
;; 'default or 'auto doesn't work on macOS
(plist-put org-format-latex-options :foreground "Black")
(plist-put org-format-latex-options :background "Transparent")

;;;;; functions for changing preview state
(defvar-local cys/org-latex-preview-is-on t
  "Whether LaTeX preview is on in the current buffer.")
(defun cys/org-show-latex-preview ()
  "Show preview of LaTeX fragments in the buffer."
  (interactive)
  (org-latex-preview '(16))
  (setq-local cys/org-latex-preview-is-on t))
(defun cys/org-hide-latex-preview ()
  "Hide preview of LaTeX fragments in the buffer."
  (interactive)
  (org-latex-preview '(64))
  (setq-local cys/org-latex-preview-is-on nil))
(defun cys/org-toggle-latex-preview ()
  "Toggle preview of LaTeX fragments in the buffer."
  (interactive)
  (if cys/org-latex-preview-is-on
      (cys/org-hide-latex-preview)
    (cys/org-show-latex-preview)))

;;;; inline images
(setq org-startup-with-inline-images t)

;;;; finish up
(provide 'org-config)

;;; org-config.el ends here
