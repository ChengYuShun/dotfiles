;;; cys/shorthands.el -- my shorthand system

;;; Code:

;;;; core stuff

(require 'cys/common-utils)

(define-minor-mode cys/shorthands-mode
  "Minor mode for various shorthands."
  :global nil
  (unless cys/shorthands-mode
    ;; disable sub-modes when disabling this mode.
    (cys/shorthands-latex-mode -1)))

(defvar cys/shorthands-mode-map (make-sparse-keymap))

(dolist (pair '(("C-s <tab>" . cys/shorthands-tab)
                ("C-s TAB"   . cys/shorthands-tab)
                ("C-s <escape>" . cys/shorthands-escape)
                ("C-s ESC"      . cys/shorthands-escape)
                ("C-s <return>" . cys/shorthands-return)
                ("C-s RET"      . cys/shorthands-return)
                ("S-<escape>" . cys/shorthands-tab)
                ("S-ESC"      . cys/shorthands-tab)
                ("S-<return>" . cys/shorthands-return)
                ("S-RET"      . cys/shorthands-return)
                ("C-<escape>" . cys/shorthands-escape)
                ("C-ESC"      . cys/shorthands-escape)))
  (keymap-set cys/shorthands-mode-map (car pair) (cdr pair)))

(cys/alist-set minor-mode-map-alist
               'cys/shorthands-mode cys/shorthands-mode-map)

(defvar-local cys/shorthands-stack nil
  "This is a list of handlers.  Each handler is a function that
takes one of the possible actions:

- `escape'

- `return'

- `tab'

Each handler will also return one of the values:

- nil, meaning that no action is taken at this level, this
  handler should be removed from the stack, and we should find
  the next handler capable of handling this action.

- t, meaning that action has just been taken, and no further
  action is required.

- `finished', meaning that some action has been taken, and no
  further action is required, but this handler has done its job,
  and should be removed from the stack.")

(defun cys/shorthands-stack-push (handler)
  "Push a handler into `cys/shorthands-stack'."
  (setq-local cys/shorthands-stack (cons handler cys/shorthands-stack)))

(defun cys/shorthands-stack-pop ()
  "Pop a handler from `cys/shorthands-stack'."
  (setq-local cys/shorthands-stack (cdr cys/shorthands-stack)))

(defun cys/shorthands-perform-action (action)
  "Return t if anything has been done, and nil otherwise."
  (let ((action-performed nil)
        (handler)
        (res))
    (while (and (not action-performed)
                cys/shorthands-stack)
      (setq handler (car cys/shorthands-stack))
      (setq res (funcall handler action))
      (cond
       ((eq res nil)
        (cys/shorthands-stack-pop))
       ((eq res t) (setq action-performed t))
       ((eq res 'finished)
        (cys/shorthands-stack-pop)
        (setq action-performed t))
       (t (error "undefined return value"))))
    action-performed))

(defun cys/shorthands-escape ()
  "Trigger the escape action."
  (interactive)
  (cys/shorthands-perform-action 'escape))

(defun cys/shorthands-return ()
  "Trigger the return action."
  (interactive)
  (cys/shorthands-perform-action 'return))

(defun cys/shorthands-tab ()
  "Trigger the tab action."
  (interactive)
  (cys/shorthands-perform-action 'tab))

(defun cys/shorthands-return-or-return ()
  "Trigger the return action, or insert a newline if no handler is
present."
  (interactive)
  (unless (cys/shorthands-return)
    (insert "\n")))

(defun cys/shorthands-tab-or-tab ()
  "Trigger the tab action, or insert a tab if no handler is present."
  (interactive)
  (unless (cys/shorthands-tab)
    (insert "\t")))

(defun cys/shorthands-marker-jumper (marker &optional action)
  "Add a marker jumper to the stack.  The default bound action is
tab.  ACTION may be a list of acceptable actions."
  (let* ((action (or action 'tab))
         (action (if (listp action) action
                   (list action))))
    (cys/shorthands-stack-push
     `(lambda (input-action)
        (cond
         ((member input-action (quote ,action))
          (goto-char ,marker)
          (unless (eq action 'escape)
            (evil-insert-state 1))
          'finished)
         (t nil))))))

;; (defun cys/shorthands-insert-text-with-marker (&rest strings)
;;   "Return a list of markers, one after each string inserted, in
;; reverse order.  Finish at the current marker."
;;   (let ((current-marker (point-marker))
;;         (markers nil))
;;     (dolist (string strings)
;;       (insert string)
;;       (setq markers (cons (point-marker) markers)))
;;     (setq markers (reverse markers))
;;     (goto-char current-marker)))

(defun cys/shorthands-insert-text-with-marker-jumper (&rest string-or-marker)
  "End at the current position."
  (let ((current-marker (point-marker))
        (marker-list nil)
        (action nil)
        (marker nil))
    (dolist (x string-or-marker)
      (if (stringp x)
          (insert x)
        (setq marker-list (cons (cons x (point-marker)) marker-list))))
    (dolist (action-marker-pair marker-list)
      (setq action (car action-marker-pair)
            marker (cdr action-marker-pair))
      (cys/shorthands-marker-jumper marker action))
    (goto-char current-marker)))

(defun cys/shorthands-insert-brackets (left middle-action right end-action)
  "Return t if handlers for actions are successfully added.
Return nil if we were in visual state."
  (cond
   ((eq evil-state 'visual)
    (goto-char evil-visual-beginning)
    (insert left)
    (goto-char evil-visual-end)
    (insert right)
    (evil-normal-state 1)
    nil)
   (t (cys/shorthands-insert-text-with-marker-jumper
       left middle-action right end-action)
      t)))

(defun cys/shorthands-insert-surrounded (beg middle-action end end-action)
  "Return t if handlers for actions are successfully added.  Return
nil if we were is visual state.  Please do not add new lines
around BEG and END."
  (cond
   ((eq evil-state 'visual)
    (goto-char evil-visual-beginning)
    (move-beginning-of-line nil)
    (insert beg)
    (insert "\n")
    (goto-char evil-visual-end)
    (move-end-of-line nil)
    (insert "\n")
    (insert end)
    (if (equal (buffer-end 1) (point))
        (insert "\n")
      (forward-char))
    (evil-normal-state 1))
   (t (save-excursion
        (unless (equal (point) (line-beginning-position))
          (insert "\n"))
        (insert beg)
        (insert "\n")
        (save-excursion
          (insert "\n")
          (insert end)
          (if (or (not (eql (point) (line-end-position)))
                  (eql (point) (buffer-end 1)))
              (insert "\n")
            (forward-char))
          (cys/shorthands-marker-jumper (point-marker) end-action))
        (cys/shorthands-marker-jumper (point-marker) middle-action)))))

;;;; LaTeX stuff

(define-minor-mode cys/shorthands-latex-mode
  "Mode for various LaTeX shorthands."
  :global nil
  (when cys/shorthands-latex-mode
    ;; enable super-mode when enabling this mode
    (cys/shorthands-mode 1)))

(defvar cys/shorthands-latex-mode-map (make-sparse-keymap))

(dolist (pair '(("C-s" . nil)
                ("C-s m i"   . cys/shorthands-latex-begin-inline-math)
                ("C-s m m"   . cys/shorthands-latex-begin-long-math)
                ("C-s a l n" . cys/shorthands-latex-begin-align)
                ;; matrices
                ("C-s p m a t" . cys/shorthands-latex-begin-pmat)
                ("C-s v m a t" . cys/shorthands-latex-begin-vmat)
                ;; sophisticated macros
                ("C-s f r c"  . cys/shorthands-latex-frac)
                ("C-s a b s"  . cys/shorthands-latex-abs)
                ("C-s n r m"  . cys/shorthands-latex-norm)
                ;; simple unary macros
                ("C-s m r m"   . cys/shorthands-latex-mathrm)
                ("C-s m b b"   . cys/shorthands-latex-mathbb)
                ("C-s m c a l" . cys/shorthands-latex-mathcal)
                ("C-s m b f"   . cys/shorthands-latex-mathbf)
                ("C-s u d l"   . cys/shorthands-latex-underline)
                ("C-s t x t"   . cys/shorthands-latex-text)
                ("C-s o p n"   . cys/shorthands-latex-operatorname)
                ("C-s s q t"   . cys/shorthands-latex-sqrt)
                ;; constant strings
                ("C-s `"   . cys/shorthands-latex-cdot)
                ("C-s ^"   . cys/shorthands-latex-cdots)
                ("C-s . ."   . cys/shorthands-latex-ldots)
                ("C-s R e" . cys/shorthands-latex-re)
                ("C-s I m" . cys/shorthands-latex-im)))
  (keymap-set cys/shorthands-latex-mode-map (car pair) (cdr pair)))

(cys/alist-set minor-mode-map-alist
               'cys/shorthands-latex-mode cys/shorthands-latex-mode-map)

(defun cys/shorthands-latex-begin-inline-math ()
  (interactive)
  (when (cys/shorthands-insert-brackets
         "\\(" 'tab "\\)" '(tab escape))
    (cys/shorthands-tab)))

(defun cys/shorthands-latex-begin-long-math ()
  (interactive)
  (cys/shorthands-insert-surrounded
   "\\begin{equation*}" 'tab "\\end{equation*}" 'escape)
  (cys/shorthands-tab))
  ;; (cond
  ;;  ((eq evil-state 'visual)
  ;;   ;; insert "\\begin{equation*}\n" at the first line of selection
  ;;   (goto-char evil-visual-beginning)
  ;;   (move-beginning-of-line nil)
  ;;   (insert "\\begin{equation*}\n")
  ;;   ;; insert "\n\\end{equation*}" at the end of the last line of selection
  ;;   (goto-char evil-visual-end)
  ;;   (move-end-of-line nil)
  ;;   (insert "\n\\end{equation*}")
  ;;   (if (eql (buffer-end 1) (point))
  ;;       (insert "\n")
  ;;     (forward-char))
  ;;   (unless (eql (point) (line-end-position))
  ;;     (save-excursion (insert "\n"))))
  ;;  (t
  ;;   (if (equal (point) (line-beginning-position))
  ;;       (insert "\\begin{equation*}\n")
  ;;     (insert "\n\\begin{equation*}\n"))
  ;;   (save-excursion
  ;;     (if (not (equal (point) (line-end-position)))
  ;;         (insert "\n\\end{equation*}\n")
  ;;       (insert "\n\\end{equation*}")
  ;;       (if (eql (buffer-end 1) (point))
  ;;           (insert "\n")
  ;;         (forward-char)))
  ;;     (cys/shorthands-marker-jumper (point-marker) 'escape))))
  ;; (evil-insert-state 1))

(defun cys/shorthands-latex-align-handler (action)
  (cond
   ((eq action 'tab) (cys/insert-with-space-or-nothing-before "&") t)
   ((eq action 'return) (cys/insert-with-space-or-nothing-before "\\\\\n") t)
   (t nil)))

(defun cys/shorthands-latex-begin-align ()
  (interactive)
  (cys/shorthands-insert-surrounded
   "\\begin{align*}" 'tab "\\end{align*}" 'escape)
  (cys/shorthands-tab)
  (cys/shorthands-stack-push #'cys/shorthands-latex-align-handler))

(defun cys/shorthands-latex-mat-handler (action)
  (cond
   ((eq action 'tab) (cys/insert-with-space-or-nothing-before "&& ") t)
   ((eq action 'return) (cys/insert-with-space-or-nothing-before "\\\\\n") t)
   (t nil)))

(defun cys/shorthands-latex-begin-pmat ()
  (interactive)
  (cys/shorthands-insert-text-with-marker-jumper
   "\\begin{pmatrix}\n" 'tab "\n\\end{pmatrix}" 'escape)
  (cys/shorthands-tab)
  (cys/shorthands-stack-push #'cys/shorthands-latex-mat-handler))

(defun cys/shorthands-latex-begin-vmat ()
  (interactive)
  (cys/shorthands-insert-text-with-marker-jumper
   "\\begin{vmatrix}\n" 'tab "\n\\end{vmatrix}" 'escape)
  (cys/shorthands-tab)
  (cys/shorthands-stack-push #'cys/shorthands-latex-mat-handler))

;; sophisticated macros

(defun cys/shorthands-latex-frac ()
  (interactive)
  (cys/shorthands-insert-text-with-marker-jumper
   "\\frac{" 'tab "}{" 'tab "}" 'tab)
  (cys/shorthands-tab))

(defun cys/shorthands-latex-abs ()
  (interactive)
  (when (cys/shorthands-insert-brackets "\\lvert " 'tab " \\rvert" 'tab)
    (cys/shorthands-tab)
    (evil-insert-state 1)))

(defun cys/shorthands-latex-norm ()
  (interactive)
  (when (cys/shorthands-insert-brackets "\\lVert " 'tab " \\rVert" 'tab)
    (cys/shorthands-tab)
    (evil-insert-state 1)))

;; a lot of unary macros
(defun cys/shorthands-latex-apply-macro (macro-name)
  (interactive)
  (when (cys/shorthands-insert-brackets
         (concat "\\" macro-name "{") 'tab "}" 'tab)
    (cys/shorthands-tab)))
(defun cys/shorthands-latex-mathrm ()
  (interactive)
  (cys/shorthands-latex-apply-macro "mathrm"))
(defun cys/shorthands-latex-mathbb ()
  (interactive)
  (cys/shorthands-latex-apply-macro "mathbb"))
(defun cys/shorthands-latex-mathbf ()
  (interactive)
  (cys/shorthands-latex-apply-macro "mathbf"))
(defun cys/shorthands-latex-mathcal ()
  (interactive)
  (cys/shorthands-latex-apply-macro "mathcal"))
(defun cys/shorthands-latex-text ()
  (interactive)
  (cys/shorthands-latex-apply-macro "text"))
(defun cys/shorthands-latex-underline ()
  (interactive)
  (cys/shorthands-latex-apply-macro "underline"))
(defun cys/shorthands-latex-operatorname ()
  (interactive)
  (cys/shorthands-latex-apply-macro "operatorname"))
(defun cys/shorthands-latex-sqrt ()
  (interactive)
  (cys/shorthands-latex-apply-macro "sqrt"))

;; a lot of strings
(defun cys/shorthands-latex-cdot ()
  (interactive)
  (insert "\\cdot"))
(defun cys/shorthands-latex-cdots ()
  (interactive)
  (insert "\\cdots"))
(defun cys/shorthands-latex-ldots ()
  (interactive)
  (insert "\\ldots"))
(defun cys/shorthands-latex-re ()
  (interactive)
  (insert "\\operatorname{Re}"))
(defun cys/shorthands-latex-im ()
  (interactive)
  (insert "\\operatorname{Im}"))

;;;; finish up
(provide 'cys/shorthands)
