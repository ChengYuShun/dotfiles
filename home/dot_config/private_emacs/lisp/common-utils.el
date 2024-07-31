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

;;;; random symbol
(defun cys/randsym (&optional length)
  "Generate a random symbol with LENGTH.

If LENGTH is nil, it is defaulted to 12"
  (let ((res "")
        (length (or length 12)))
    (dotimes (i length (make-symbol res))
      (setq res (concat res (string (+ ?A (random 26))))))))

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
        (key-var (cys/randsym))
        ;; the value
        (val-var (cys/randsym))
        ;; place to which ref-var refers
        (dat-place-var (cys/randsym))
        ;; reference to the rest of alist to be searched, e.g. (cdr
        ;; DAT-PLACE-VAR)
        ;;
        ;; This value may be evaluated, so it must not contain other lists.
        (rest-ref-var (cys/randsym))
        ;; whether we have found the cons
        (found-var (cys/randsym)))
    `(let ((,key-var ,key-exp)
           (,val-var ,val-exp)
           (,dat-place-var nil)
           (,rest-ref-var ',alist-place)
           (,found-var nil))
       ;; find the cons
       (while (and (not ,found-var) (eval ,rest-ref-var))
         (if (equal (caar (eval ,rest-ref-var)) ,key-var)
             (setq ,found-var t)
           (setq ,dat-place-var (eval ,rest-ref-var))
           (setq ,rest-ref-var '(cdr ,dat-place-var))))
       ;; update the cons
       (if (or ,val-var (eval ,no-delete-exp))
           (if ,found-var
               (setf (cdar (eval ,rest-ref-var)) ,val-var)
             (eval (,'\` (setf (,'\, ,rest-ref-var)
                               (list (cons ,key-var ,val-var))))))
         (when ,found-var
           (eval (,'\` (setf (,'\, ,rest-ref-var)
                             (cdr (eval ,rest-ref-var))))))))))
