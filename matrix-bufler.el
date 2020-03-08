;;; matrix-bufler.el --- Bufler configuration for Matrix  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; NOTE: This is a WIP.

;; This provides a Bufler configuration for Matrix.  It needs more
;; work to not interfere with the main Bufler config; ideally there
;; should be a `matrix-bufler' command that would open a Bufler window
;; with its own configuration.  Shouldn't be too hard, but it might
;; need a few tweaks to Bufler to make easier.

;; For Bufler, see <https://github.com/alphapapa/bufler.el>.

;;; Code:

;;;###autoload
(when (require 'bufler nil t)

  (bufler-defauto-group matrix
    (when (or (member (buffer-local-value 'major-mode buffer)
                      '(matrix-client-mode matrix-room-list-mode))
              (string-match-p (rx bos "*Matrix") (buffer-name buffer)))
      "Matrix"))

  (bufler-defauto-group matrix-room
    (when (member (buffer-local-value 'major-mode buffer) '(matrix-client-mode))
      "Rooms"))

  (bufler-defauto-group matrix-room-direct
    (when (matrix-room-direct-p (buffer-local-value 'matrix-client-room buffer))
      "Direct"))

  (bufler-defauto-group matrix-room-tag
    (when (member (buffer-local-value 'major-mode buffer) '(matrix-client-mode))
      (pcase-let* ((room (buffer-local-value 'matrix-client-room buffer))
                   (`(,tags ,favorite-p ,low-priority-p)
                    (matrix-client-room-list--tags room)))
        (cond ((not (string-empty-p tags)) tags)
              (favorite-p "Favorites")
              (low-priority-p "Low priority")
              ((matrix-room-direct-p (oref room id) (car matrix-client-sessions)) "Direct")
              (t "Other")))))

  (bufler-define-column "Members"
    (ignore depth)
    (when-let* ((room (buffer-local-value 'matrix-client-room buffer)))
      (file-size-human-readable (length (oref room members)))))

  (bufler-define-column "Last message"
    (ignore depth)
    (when-let* ((room (buffer-local-value 'matrix-client-room buffer))
                (timeline (oref room timeline)))
      (format-time-string "%Y-%m-%d %H:%M:%S" (matrix-client-event-timestamp (car timeline)))))

  (bufler-define-column "Buffer" nil
    ;; MAYBE: Move indentation back to `bufler-list'.  But this seems to
    ;; work well, and that might be more complicated.
    (if (buffer-local-value 'matrix-client-room buffer)
        (let* ((room (buffer-local-value 'matrix-client-room buffer))
               (matrix-client-show-room-avatars-in-buffer-names nil))
          (matrix--room-display-name room))
      (buffer-name buffer)))

  (bufler-define-column "🐱" nil
    ;; MAYBE: Move indentation back to `bufler-list'.  But this seems to
    ;; work well, and that might be more complicated.
    (if-let* ((room (buffer-local-value 'matrix-client-room buffer))
              (avatar (oref room avatar))
              (resized (matrix-client-resize-avatar avatar)))
        (concat (make-string (* 2 depth) ? ) (substring resized 0 1))
      (concat (make-string (* 2 depth) ? ) " ")))

  (defun matrix-client-not-matrix-buffer-p (buffer)
    "Return non-nil if BUFFER is NOT a Matrix buffer."
    (not (or (member (buffer-local-value 'major-mode buffer)
                     '(matrix-client-mode matrix-room-list-mode))
             (string-match-p (rx bos "*Matrix") (buffer-name buffer)))))

  (defun matrix-bufler ()
    "Show Matrix rooms in Bufler."
    (interactive)
    ;; FIXME: These won't remain in-effect on refresh because Bufler doesn't save these buffer-locally.
    (let ((bufler-groups (bufler-defgroups
                           (group ;; (bufler-group 'auto-matrix)
                            (bufler-group 'auto-matrix-room)
                            (bufler-group 'auto-matrix-room-tag))
                           (group (name-match "*Special*" (rx bos "*")))))
          (bufler-filter-fns '(matrix-client-not-matrix-buffer-p))
          ;; FIXME: Room avatars mess up the calculation of the length of the room name.  Not easy to fix.
          ;; (bufler-columns '("Room"  "Members" "Last message"))
          (bufler-columns '("🐱" "Buffer" "Members"))
          ;; Try to make the avatars in this list display at the same width as a character
          ;; so the rooms without avatars line up with the ones that have avatars.
          (matrix-client-room-avatar-in-buffer-name-size (* 2 (default-font-width))))
      (save-window-excursion
        (bufler))
      (when-let* ((window (display-buffer-in-side-window (get-buffer "*Bufler*")
                                                         '((side . right)))))
        (set-window-parameter window 'delete-window #'ignore)
        (select-window window)))))

;;;; Footer

(provide 'matrix-bufler)

;;; matrix-bufler.el ends here
