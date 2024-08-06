;;; common-utils -- common utilities.

;; Common utilities.

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

;;;; temporary file

(defmacro cys/with-temp-dir (path-var &rest body)
  "Execuate BODY with a temporary directory safely.

The temporary directory created will be automatically deleted.

PATH-VAR is a symbol that will contain the path to the temporary
directory during the execution of BODY.

The value of the last argument of BODY will be returned."

  `(let ((,path-var (make-temp-file "" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,path-var t))))

(defmacro cys/with-temp-file (path-var text &rest body)
  "Execute BODY with a temporary file safely.

The temporary file created will be automatically deleted.

PATH-VAR is a symbol that will contain the path to the temporary
file during the execution of BODY.

TEXT, if non-nil, is the text content that will be initially
inserted into the file.

The value of the last argument of BODY will be returned."

  `(let ((,path-var (make-temp-file "" nil nil text)))
     (unwind-protect
         (progn ,@body)
       (delete-file ,path-var))))

;;;; associate list

(defmacro cys/alist-set (alist-place key-exp val-exp &optional no-delete-exp)
  "Update the association list.

ALIST-PLACE is the place that stores the alist to be updated.

((eval KEY-EXP) . (eval VAL-EXP)) will be updated to the alist.

If (eval NO-DELETE-EXP) is nil, the first cons with its car being
KEY will be deleted.  If (eval NO-DELETE-EXP) is non-nil, the
cons will not be deleted."
  (let (;; the key
        (key-sym (gensym "key-"))
        ;; the value
        (val-sym (gensym "val-"))
        ;; place to which ref-var refers
        (dat-place-sym (gensym "dat-place-"))
        ;; reference to the rest of alist to be searched, e.g. (cdr
        ;; DAT-PLACE-VAR)
        ;;
        ;; This value may be evaluated, so it must not contain other lists.
        (rest-ref-sym (gensym "rest-ref-"))
        ;; whether we have found the cons
        (found-sym (gensym "found-")))
    `(let ((,key-sym ,key-exp)
           (,val-sym ,val-exp)
           (,dat-place-sym nil)
           (,rest-ref-sym ',alist-place)
           (,found-sym nil))
       ;; find the cons
       (while (and (not ,found-sym) (eval ,rest-ref-sym))
         (if (equal (caar (eval ,rest-ref-sym)) ,key-sym)
             (setq ,found-sym t)
           (setq ,dat-place-sym (eval ,rest-ref-sym))
           (setq ,rest-ref-sym '(cdr ,dat-place-sym))))
       ;; update the cons
       (if (or ,val-sym (eval ,no-delete-exp))
           (if ,found-sym
               (setf (cdar (eval ,rest-ref-sym)) ,val-sym)
             (eval (,'\` (setf (,'\, ,rest-ref-sym)
                               (list (cons ,key-sym ,val-sym))))))
         (when ,found-sym
           (eval (,'\` (setf (,'\, ,rest-ref-sym)
                             (cdr (eval ,rest-ref-sym))))))))))

(defmacro cys/alist-set-many (alist-place &rest key-val-exps)
  "Update the association list.

KEY-VAL-EXPS are key expressions and value expressions."
  (let ((macro-calls nil))
    (while key-val-exps
      (let ((key-exp (car key-val-exps))
            (val-exp (cadr key-val-exps)))
        (setq key-val-exps (cddr key-val-exps))
        (setq macro-calls
              (cons `(cys/alist-set ,alist-place ,key-exp ,val-exp)
                    macro-calls))))
    (cons 'progn macro-calls)))

;;;; finish up
(provide 'cys/common-utils)

;;; common-utils.el ends here
