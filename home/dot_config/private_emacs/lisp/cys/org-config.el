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
(require 'cys/common-utils)
(require 'simple)

;;;; basic config
(setq org-startup-indented t)
(setq org-export-with-smart-quotes t)
(setq org-startup-folded 'nofold
      org-cycle-hide-drawer-startup t)
(cys/alist-set-many org-file-apps
                    'directory 'system
                    "\\.pdf\\'" 'system)

;;;; key bindings
(define-key org-mode-map "M" nil)
(define-key visual-line-mode-map [remap org-fill-paragraph] #'ignore)
(evil-define-key 'normal org-mode-map
  [remap cys/evil-open-link] #'org-open-at-point
  [remap cys/evil-go-back] #'org-mark-ring-goto)
(dolist (pair '(("C-c l p" . org-insert-link)
                ("C-c n i g" . cys/org-roam-node-insert-global)
                ("C-c n i n" . cys/org-roam-node-insert-non-global)
                ("C-c n l p" . org-roam-link-paste)))
  (keymap-set org-mode-map (car pair) (cdr pair)))
(evil-define-key 'motion org-mode-map
  ;; link
  (kbd "t l") nil
  (kbd "t l p") #'org-insert-link
  (kbd "t l y") #'org-store-link
  (kbd "t l t") #'org-toggle-link-display
  ;; LaTeX preview
  (kbd "t m f") #'org-fragtog-mode
  (kbd "t m t") #'cys/org-toggle-latex-preview
  (kbd "t m s") #'cys/org-show-latex-preview
  (kbd "t m h") #'cys/org-hide-latex-preview
  (kbd "t m d") #'cys/org-delete-latex-preview
  ;; inline images
  (kbd "t i t") #'org-toggle-inline-images
  (kbd "t i s") #'org-display-inline-images
  (kbd "t i h") #'org-remove-inline-images
  ;; reserved for org-roam
  (kbd "t n i g") #'cys/org-roam-node-insert-global
  (kbd "t n i n") #'cys/org-roam-node-insert-non-global
  (kbd "t n l y") #'org-roam-link-store
  (kbd "t n l o") #'org-roam-link-open
  (kbd "t n l p") #'org-roam-link-paste
  (kbd "t n k i") #'cys/org-roam-diagram-insert
  (kbd "t n k c") #'cys/org-roam-diagram-modify
  (kbd "t n k d") #'cys/org-roam-diagram-delete
  (kbd "t n k x") #'cys/org-roam-diagram-finish
  (kbd "t n b") #'org-roam-buffer-toggle
  (kbd "t n r") #'cys/org-roam-node-rename
  (kbd "t n d") #'cys/org-roam-node-delete
  (kbd "t n g") #'cys/org-roam-global-toggle
  (kbd "t n a") #'cys/org-roam-agenda-toggle)

;;;; org-agenda
(setq org-deadline-warning-days 0)
(setq org-agenda-start-on-weekday nil)
(setq org-log-done t)
(evil-define-key 'motion org-agenda-mode-map
  "q" 'org-agenda-quit
  "j" 'org-agenda-next-item
  "k" 'org-agenda-previous-item
  (kbd "RET") 'org-agenda-switch-to
  (kbd "<tab>") 'org-agenda-goto)
(evil-set-initial-state 'org-agenda-mode 'motion)
(defun cys/org-agenda ()
  (interactive)
  (cys/org-roam-agenda-files-update)
  (org-agenda))

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

;;;; Latex preview

;;;;; basic settings
(setq org-startup-with-latex-preview t)
(setq org-latex-packages-alist
      '(("" "mhchem" t)
        ("" "tikz-cd" t)))
(plist-put org-format-latex-options :justify 'center)

;;;;; SVG preview
(cys/alist-set
 org-preview-latex-process-alist
 'cys-svg
 `(:programs
   ("latex" "pdftocairo" "inkscape")
   :description "pdf > svg"
   :message "you need to install the programs: latex, pdftocairo and inkscape."
   :image-input-type "pdf" :image-output-type "svg"
   :image-size-adjust
   ,(let ((scale (/ (float (string-to-number (getenv "DPI"))) 72.0)))
      (cons scale scale))
   :latex-compiler
   ("pdflatex -interaction nonstopmode -output-directory %o %f")
   :image-converter
   ("ltxpdf2svg %f %O %S '#ffffff'")))
(cys/alist-set
 org-preview-latex-process-alist
 'cys-mathjax
 `(:programs
   ("math2svg-server" "math2svg-client")
   :description "tex > svg"
   :message "you need to install math2svg"
   :image-input-type "tex" :image-output-type "svg"
   :image-size-adjust
   ,(let ((scale (/ (float (string-to-number (getenv "DPI"))) 144.0)))
      (cons scale scale))
   :image-converter
   ("math2svg-client -i %f -o %O -s %S")))
(setq org-preview-latex-image-directory
      (concat (file-name-as-directory user-emacs-directory) "ltximg/"))
(setq org-preview-latex-default-process 'cys-mathjax)
(plist-put org-format-latex-options :scale 0.9)
;; 'default or 'auto doesn't work on macOS
(plist-put org-format-latex-options :foreground "Black")
(plist-put org-format-latex-options :background "Transparent")

;;;; functions for changing preview state
(defvar-local cys/org-latex-preview-is-on t
  "Whether LaTeX preview is on in the current buffer.")
(defun cys/org-show-latex-preview ()
  "Show preview of LaTeX fragments in the buffer."
  (interactive)
  (if (eq evil-state 'visual)
      (org--latex-preview-region (marker-position evil-visual-beginning)
                                 (marker-position evil-visual-end))
    (org--latex-preview-region (point-min) (point-max))
    (setq-local cys/org-latex-preview-is-on t)))
(defun cys/org-hide-latex-preview ()
  "Hide preview of LaTeX fragments in the buffer."
  (interactive)
  (if (eq evil-state 'visual)
      (org-clear-latex-preview (marker-position evil-visual-beginning)
                               (marker-position evil-visual-end))
    (org-clear-latex-preview)
    (setq-local cys/org-latex-preview-is-on nil)))
(defun cys/org-toggle-latex-preview ()
  "Toggle preview of LaTeX fragments in the buffer."
  (interactive)
  (if cys/org-latex-preview-is-on
      (cys/org-hide-latex-preview)
    (cys/org-show-latex-preview)))
(defun cys/org-delete-latex-preview ()
  (interactive)
  (if (eq evil-state 'visual)
      (org-delete-latex-preview (marker-position evil-visual-beginning)
                                (marker-position evil-visual-end))
    (org-delete-latex-preview)
    (setq-local cys/org-latex-preview-is-on nil)))

;;;; inline images
(setq org-startup-with-inline-images t)

;;;; icalendar
(setq org-icalendar-use-scheduled '(todo-start event-if-todo)
      org-icalendar-timezone "Europe/London")

;;;; finish up
(provide 'cys/org-config)

;;; org-config.el ends here
