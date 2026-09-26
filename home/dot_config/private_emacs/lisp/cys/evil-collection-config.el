;; -*- lexical-binding: nil -*-

(defmacro cys/evil-collection-override-key (mode state keymap
                                            key def &rest bindings)
  "This macro does almost exactly what `evil-define-key' does, except that
it is able to override settings in `evil-collection-init'.  MODE should
be in `evil-collection-mode-list'.

There is another important difference between this macro and
`evil-define-key': KEYMAP may either be a symbol or a quoted symbol, and
strictly can't be anything else.  Even though the documentation of
`evil-define-key' also forbids KEYMAP to be anything else, its
definition actually allows it to be an expression that returns the
keymap.

In general, what this macro achieves it is not something that can be
conveniently done.  `evil-define-key' adds a hook in
`after-load-functions'; `evil-collection-init' adds the setup function
to `after-load-alist' via `with-eval-after-load'.  For some
reason (undocumented), `after-load-functions' are run before
`after-load-alist', so `evil-define-key' will always be overriden by the
stuff set by Evil Collection.  The function `evil-collection-define-key'
is just a wrapper around `evil-define-key*', so that won't work either.

The idiomatic solution for this has been to set the `:config' key in
`use-package'.  However, this does not work for files that are not
packages, like org-agenda or help.

Therefore I wrote this macro.  It works by adding a helper function
to `evil-collection-setup-hook', instead of `after-load-functions' or
`after-load-alist', provided that Evil Collection supports the mode.  If
Evil Collection does not support the mode, we simply fallback to
`evil-define-key'."
  (declare (indent defun))  ;; See Info, Elisp, Macros, Indenting Macros.
  (cond
   ((not (member mode evil-collection-mode-list))
    `(evil-define-key ,state ,keymap ,key ,def ,@bindings))
   ((eq (car-safe keymap) 'quote)
    `(evil-define-key ,state ,keymap ,key ,def ,@bindings))
   ((not (symbolp keymap))
    (error "KEYMAP must be a symbol or a quoted symbol, but get: %s" keymap))
   (t
    `(evil-with-delay (and
                       (featurep ',(intern (format "evil-collection-%s" mode)))
                       (boundp ',keymap)
                       (keymapp ,keymap))
         (evil-collection-setup-hook
          t nil
          ,(symbol-name (gensym (format "cys/evil-collection-override-%s-"
                                        mode))))
       (evil-define-key ,state ,keymap ,key ,def ,@bindings)))))

;; dired
(cys/evil-collection-override-key dired 'normal dired-mode-map
  (kbd "SPC") nil
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-find-file)

;; flycheck
(cys/evil-collection-override-key flycheck 'motion flycheck-mode-map
  (kbd "]d") #'flycheck-next-error
  (kbd "[d") #'flycheck-previous-error)

;; help
(cys/evil-collection-override-key help 'normal help-mode-map
  (kbd "SPC") nil
  [remap cys/evil-go-back] #'help-go-back)

;; info
(cys/evil-collection-override-key info 'normal Info-mode-map
  (kbd "SPC") nil
  [remap cys/evil-open-link] #'Info-follow-nearest-node
  [remap cys/evil-go-up] #'Info-up
  [remap cys/evil-goto-next] #'Info-next
  [remap cys/evil-goto-prev] #'Info-prev
  [remap cys/evil-go-back] #'Info-last)

;; man
(cys/evil-collection-override-key man 'normal Man-mode-map
  (kbd "SPC") nil
  (kbd "u") #'scroll-down-command
  (kbd "d") #'scroll-up-command)

;; outline
(cys/evil-collection-override-key outline 'motion outline-minor-mode-map
  (kbd "<tab>") nil
  (kbd "<tab>") #'outline-toggle-subtree
  (kbd "C-i") #'outline-toggle-subtree
  (kbd "<backtab>") nil
  (kbd "<backtab>") #'outline-show-children)

;; term
(cys/evil-collection-override-key term nil term-mode-map
  (kbd "RET") nil
  (kbd "C-j") nil
  (kbd "M-x") nil)
(cys/evil-collection-override-key term 'normal term-mode-map
  (kbd "RET") #'term-send-input)

;; view
(cys/evil-collection-override-key view 'normal view-mode-map
  (kbd "SPC") nil)

;; Initialize evil-collection.
(evil-collection-init)

(provide 'cys/evil-collection-config)
