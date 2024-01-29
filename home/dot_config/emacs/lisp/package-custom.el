;;; package-custom --- Customized functions for package.el.

;;; License:

;; Copyright (C) 2022  Yushun Cheng <chengys@disroot.org>
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

;;; Commentary:

;; Customized functions for package.el.

;;; Code:

;;;; Load package.el.
(require 'package)

;;;; Functions.
(defun package-install-smart (package-name)
  "Install PACKAGE-NAME (a symbol) smartly."
  (if (package-installed-p package-name)
      (add-to-list 'package-selected-packages package-name)
    (unless (locate-library (symbol-name package-name))
      (unless package-archive-contents
        (package-refresh-contents))
      (package-install package-name)
      (add-to-list 'package-selected-packages package-name))))

(defun package-update (&rest package-names)
  "Update PACKAGE-NAMES or all packages installed.
Return the list updated."
  (unless package-archive-contents
    (package-refresh-contents))
  (let ((ret nil) (name) (desc))
    (dolist (package-cons package-alist)
      (setq name (car package-cons))
      (setq desc (nth 1 package-cons))
      (when (and (or (null package-names)
                     (member name package-names))
                 (not (equal (package-desc-version desc)
                             (package-desc-version
                              (nth 1 (assoc name package-archive-contents))))))
        (package-reinstall name)
        (setq ret (cons name ret))))
    ret))

(provide 'package-custom)
;;; package-custom.el ends here
