;;; org-roam-config -- my config for org-roam.

;; My configurations for org-roam.

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

;;;; basic settings
(require 'cys/common-utils)
(org-roam-db-autosync-enable)

;;;; template
(let ((common-head (concat ":PROPERTIES:\n"
                           ":CREATION_TIME: %<%FT%T%z>\n"
                           ":END:\n"
                           "#+title: ${title}\n"))
      (file-name "${slug-dash}${id}.org"))
  (setq org-roam-capture-templates
        `(("g" "global" plain "%?"
           :target (file+head ,file-name
                              ,(concat common-head "#+filetags: :global:\n"))
           :unnarrowed t)
          ("n" "non-global" plain "%?"
           :target (file+head ,file-name ,common-head)
           :unnarrowed t))))

;;;; interactive commands
(defun cys/org-roam-filter-global (node)
  (member "global" (org-roam-node-tags node)))

(defun cys/org-roam-node-find-global ()
  "Find a global node."
  (interactive)
  (org-roam-node-find nil nil #'cys/org-roam-filter-global nil
                      :templates `(,(nth 0 org-roam-capture-templates))))

(defun cys/org-roam-node-insert-global ()
  "Insert a global node."
  (interactive)
  (org-roam-node-insert #'cys/org-roam-filter-global
                        :templates `(,(nth 0 org-roam-capture-templates))))

(defun cys/org-roam-node-insert-non-global ()
  "Insert a non-global node."
  (interactive)
  (org-roam-node-insert #'(lambda (node) nil)
                        :templates `(,(nth 1 org-roam-capture-templates))))

(defun cys/org-roam-node-delete (&optional show-prompt)
  "Delete a node."
  (interactive "p")
  (when (or (not show-prompt)
            (yes-or-no-p "Do you really want to delete this note?"))
    (save-buffer)
    (let ((file-name (buffer-file-name)))
      (kill-buffer)
      (delete-file file-name))))

(defun cys/org-roam-global-toggle ()
  "Toggle the global tag for the node at point."
  (interactive)
  (let ((node (org-roam-node-at-point)))
    (if (member "global" (org-roam-node-tags node))
        (org-roam-tag-remove '("global"))
      (org-roam-tag-add '("global")))))

;;;; common lisp methods
(cl-defmethod org-roam-node-slug-dash ((node org-roam-node))
  "Return the title line of NODE."
  (let ((slug (org-roam-node-slug node)))
    (if (and slug (not (equal slug "")))
        (concat slug "-")
      "")))

;;; Finish up.
(provide 'cys/org-roam-config)

;;; org-roam-config.el ends here.
