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
  "Delete a node at the current buffer."
  (interactive "p")
  (when (or (not show-prompt)
            (yes-or-no-p "Do you really want to delete this note?"))
    (save-buffer)
    (cys/org-roam-diagram-delete nil t)
    (let ((file-name (buffer-file-name)))
      (kill-buffer)
      (delete-file file-name))))

(defun cys/org-roam-global-toggle ()
  "Toggle the global tag for the node at point."
  (interactive)
  (let ((node (org-roam-node-at-point t)))
    (if (member "global" (org-roam-node-tags node))
        (org-roam-tag-remove '("global"))
      (org-roam-tag-add '("global")))))

;;;; common subroutines

(cl-defmethod org-roam-node-slug-dash ((node org-roam-node))
  "Return the title line of NODE."
  (let ((slug (org-roam-node-slug node)))
    (if (and slug (not (equal slug "")))
        (concat slug "-") "")))

;;;; diagram with Krita

(defvar-local cys/org-roam-diagram-state nil
  "The state of diagram editing in the current buffer.

It can take one of three forms.

If it is nil, it means the diagram associated with the buffer, if
any, is not being edited.  This is also the state if no diagram
is currently associated with the buffer, or trying to be
associated with the buffer.


If it is of the form (inserting \"dir-path\" \"file-name\"), it
means that currently a dot kra document in directory \"dir-path\"
with file name \"file-name\" is being edited to be inserted later
on.

If it is of the form (modifying \"dir-path\" \"file-name\"), it
means that currently a dot kra document in directory \"dir-path\"
with file name \"file-name\" is being edited to override an
existing diagram associated with the buffer.

For the last two cases, the directory will be removed altogether
when the buffer is killed.")

(defvar cys/org-roam-diagram-path (concat cys/org-roam-repo "/diagrams"))

(defvar cys/org-roam-diagram-template-path
  (concat cys/org-roam-repo "/diagram-template.kra"))

(cl-defmethod cys/org-roam-node-diagram-path ((node org-roam-node))
  (concat cys/org-roam-diagram-path "/" (org-roam-node-id node) ".png"))

(defun cys/org-roam-diagram-krita-open (file-path &optional file-layer)
  "Open Krita to edit a specific file.

If FILE-LAYER is non-nil, it will be specified as a file layer.

This function will return a numeric exit status as soon as Krita
starts."
  (let* ((quoted-abs-path (shell-quote-argument (file-truename file-path)))
         (following-args quoted-abs-path)
         (command ""))
    (when file-layer
      (setq following-args
            (concat "--file-layer "
                    (shell-quote-argument (file-truename file-layer))
                    " "
                    following-args)))
    (if (eq system-type 'darwin)
        (setq command (concat "open -a krita --args " following-args))
      (setq command (concat "setsid krita " following-args)))
    (message "command: %s" command)
    (call-process-shell-command command)))

(defun cys/org-roam-diagram-kra-to-png (src-path dst-path &optional overwrite)
  "Convert a dot kra file at SRC-PATH to a PNG at DST-PATH.

If OVERWRITE is nil, do not overwrite if DST-PATH already exists
as a file; otherwise, overwrite the file at DST-PATH, if any."
  (when (file-exists-p dst-path)
    (if overwrite
        (delete-file dst-path)
      (error "DST-PATH already exists as a file.")))
  (cys/with-temp-dir mid-dir
    (let* ((quoted-src-path (shell-quote-argument (file-truename src-path)))
           (quoted-dst-path (shell-quote-argument (file-truename dst-path)))
           (quoted-mid-1-path (shell-quote-argument
                               (file-truename (concat mid-dir "/mid-1.png"))))
           (quoted-mid-2-path (shell-quote-argument
                               (file-truename (concat mid-dir "/mid-2.png"))))
           (command-1 (concat "krita --export-filename " quoted-mid-1-path
                              " --export " quoted-src-path))
           (command-2 (concat "png-rm-black-bg "
                              quoted-mid-1-path " " quoted-mid-2-path))
           (command-3 (concat "magick convert "
                              quoted-mid-2-path
                              " -trim "
                              "-alpha set -bordercolor none -border 0x20 "
                              "-quality 105 "
                              quoted-dst-path)))
      (call-process-shell-command command-1)
      (call-process-shell-command command-2)
      (call-process-shell-command command-3))))

(defun cys/org-roam-diagram-interrupt (&optional buffer prompt-p)
  "Remove the diagram being edited in BUFFER, if any.

If BUFFER is nil, the current buffer will be used.

If PROMPT-P is non-nil, prompt the user for whether the diagram
currently being edited (if any) should be deleted."
  (let ((buffer (or buffer (current-buffer)))
        (prompt (concat "Delete the diagram currently being edited "
                        "associated with this buffer? ")))
    (cys/with-current-buffer-visible buffer
      (when (and cys/org-roam-diagram-state
                 (or (not prompt-p) (yes-or-no-p prompt)))
        (let ((dir (cadr cys/org-roam-diagram-state)))
          (unwind-protect (delete-directory dir t)
            (setq-local cys/org-roam-diagram-state nil)))))))

(defun cys/org-roam-diagram-delete (&optional buffer no-prompt)
  "Remove the diagram associated with BUFFER, if any.

If BUFFER is nil, the current buffer will be used.

If NO-PROMPT is nil, prompt the user for whether the diagram
currently being edited (if any) and the diagram associated with
BUFFER of the current buffer (if any) should be deleted."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (cys/with-current-buffer-visible buffer
      (cys/org-roam-diagram-interrupt buffer (not no-prompt))
      (let ((diagram-path (cys/org-roam-node-diagram-path
                           (org-roam-node-at-point t)))
            (prompt "Delete the diagram associated with this buffer? "))
      (when (and (file-exists-p diagram-path)
                 (or no-prompt (yes-or-no-p prompt)))
        (delete-file diagram-path))))))

(defun cys/org-roam-diagram-edit (modify-p)
  "Edit diagram associated with the current node.

If MODIFY-P is nil, we will insert a new diagram associated with
the current node.  If MODIFY-P is nil, we will edit the diagram
associated with the current node."
  (when cys/org-roam-diagram-state
    (error "There is a diagram currently being edited."))
  (let* ((node (org-roam-node-at-point t))
         (diagram-path (cys/org-roam-node-diagram-path node))
         (diagram-exists-p (file-exists-p diagram-path)))
    (when (and (not modify-p) diagram-exists-p)
      (error "Diagram already exists."))
    (when (and modify-p (not diagram-exists-p))
      (error "No diagram exists."))
    (let* ((tmp-dir-prefix (concat "org-roam-diagram-"
                                   (if modify-p "modify-" "insert-")))
           (tmp-dir-path (make-temp-file tmp-dir-prefix t)))
      (unwind-protect
          (let ((tmp-path (concat tmp-dir-path "/diagram.kra"))
                (edit-mode (if modify-p 'modifying 'inserting)))
            (copy-file cys/org-roam-diagram-template-path tmp-path)
            (cys/org-roam-diagram-krita-open tmp-path
                                             (when modify-p diagram-path))
            (setq-local cys/org-roam-diagram-state
                        (list edit-mode tmp-dir-path "diagram.kra")))
        (unless cys/org-roam-diagram-state
          (delete-directory tmp-dir-path t))))))

(defun cys/org-roam-diagram-finish ()
  "Finish editing the diagram and yank the link."
  (interactive)
  (if cys/org-roam-diagram-state
      (let ((edit-mode (car cys/org-roam-diagram-state))
            (dir-path (cadr cys/org-roam-diagram-state))
            (file-name (caddr cys/org-roam-diagram-state))
            (node (org-roam-node-at-point t)))
        (unless (or (eq edit-mode 'inserting) (eq edit-mode 'modifying))
          (error "Invalid diagram edit mode."))
        (let ((src-path (concat dir-path "/" file-name))
              (dst-path (cys/org-roam-node-diagram-path node)))
          (cys/org-roam-diagram-kra-to-png src-path dst-path
                                           (eq edit-mode 'modifying))
          (delete-directory dir-path t)
          (setq-local cys/org-roam-diagram-state nil)
          (when (eq edit-mode 'inserting)
            (kill-new (org-link-make-string
                       (file-relative-name dst-path org-roam-directory)))
            (message "Link to the diagram copied."))))
    (message "No diagram is currently being edited in this buffer.")))

(defun cys/org-roam-diagram-insert ()
  "Insert a diagram in the current node."
  (interactive)
  (cys/org-roam-diagram-edit nil))

(defun cys/org-roam-diagram-modify ()
  "Modify the diagram associated with the current node."
  (interactive)
  (cys/org-roam-diagram-edit t))

;;; Finish up.

(provide 'cys/org-roam-config)

;;; org-roam-config.el ends here.
