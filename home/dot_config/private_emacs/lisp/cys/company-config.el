;;;; load requirements
(require 'cys/common-utils)

;;;; basic configs
(setq company-idle-delay 0.03
      company-minimum-prefix-length 1
      company-dabbrev-time-limit 0.03)
(unless (eq system-type 'windows-nt)
  (global-company-mode))

;;;; shorthand
(defvar cys/company-shorthand-alist nil
  "An alist storing common shorthands.  Each element has the
form (\"godel\" . \"Gödel\").")
(load "cys/company-shorthand-alist.el")

(defun cys/company-shorthand (command &optional arg &rest _ignored)
  "`company-mode' backend for common shorthands."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend #'cys/company-shorthand))
    (prefix (company-dabbrev--prefix))
    (candidates
     (let ((res nil)
           (val))
       (dolist (key (all-completions arg cys/company-shorthand-alist) res)
         (setq val (alist-get key cys/company-shorthand-alist)
               res (if (listp val)
                       (append val res)
                     (cons val res))))))
    (kind 'text)
    (ignore-case completion-ignore-case)
    (duplicates t)
    (sorted nil)))

;;;; finish up
(provide 'cys/company-config)
