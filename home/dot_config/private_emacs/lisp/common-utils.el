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
