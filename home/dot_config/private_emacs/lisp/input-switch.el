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

;;; Code:

(defun input-switch-ibus ()
  (defconst ibus-engine-default "xkb:us::eng" "The default I-Bus engine.")

  (defvar ibus-engine ibus-engine-default "The current I-Bus engine.")

  (defun input-switch-enter ()
    "Start using current I-Bus engine."
    (call-process "ibus" nil nil nil "engine" ibus-engine))

  (defun input-switch-exit ()
    "Stop using current I-Bus engine."
    (setq ibus-engine (substring (shell-command-to-string "ibus engine") 0 -1))
    (call-process "ibus" nil nil nil "engine" ibus-engine-default)))

(defun input-switch-fcitx5 ()
  (defvar fcitx5-active nil)

  (defun input-switch-enter ()
    (when fcitx5-active
      (call-process "fcitx5-remote" nil nil nil "-o")))

  (defun input-switch-exit ()
    (setq fcitx5-active
          (equal "2"
                 (substring (shell-command-to-string "fcitx5-remote") 0 -1)))
    (call-process "fcitx5-remote" nil nil nil "-c")))

(provide 'input-switch)
