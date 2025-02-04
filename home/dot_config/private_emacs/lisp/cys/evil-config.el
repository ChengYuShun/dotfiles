;;; evil-config -- my config for Evil.

;; My configurations for Evil.

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
(require 'simple)

(evil-define-key 'normal visual-line-mode-map
  "j" #'evil-next-visual-line
  "k" #'evil-previous-visual-line)

;;;; clean key bindings for `cys/shorthands-mode'
(dolist (keybinding '("S-<escape>" "S-ESC"
                      "C-<escape>" "C-ESC"
                      "S-<return>" "S-RET"))
  (dolist (mode (list evil-normal-state-map
                      evil-insert-state-map
                      evil-motion-state-map
                      evil-visual-state-map))
    (keymap-set mode keybinding nil)))

;;;; finish up
(provide 'cys/evil-config)

;;; evil-config.el ends here.
