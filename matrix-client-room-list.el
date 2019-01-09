;;; matrix-client-room-list.el --- Matrix Client room list  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Adam Porter

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

;;  Matrix room list!

;;; Code:

;;;; Requirements

(require 'mouse)
(require 'tabulated-list)

(require 'dash)
(require 's)

(require 'matrix-macros)
(require 'matrix-client-room)

;; Silence byte-compiler.
(declare-function #'matrix-client-resize-avatar "matrix-client.el")

;;;; Variables

;; Silence byte-compiler.
(defvar matrix-client-room-avatar-in-buffer-name-size)
(defvar matrix-client-sessions)

;;;; Customization

;;;; Commands

(defun matrix-client-room-list (&rest _ignore)
  "Show list of Matrix rooms."
  (interactive)
  (with-current-buffer (get-buffer-create "*Matrix Rooms*")
    (matrix-client-room-list-mode)
    (pop-to-buffer (current-buffer))))

(matrix-client-def-room-command rooms
  :docstring "Show room list."
  :insert (prog1 nil
            (ignore input)
            (matrix-client-room-list)))

(defun matrix-client-room-list-action (event)
  "Pop to buffer for room at EVENT or point."
  (interactive "e")
  (mouse-set-point event)
  (pop-to-buffer (oref* (tabulated-list-get-id) client-data buffer)))

(define-derived-mode matrix-client-room-list-mode tabulated-list-mode
  "Matrix Client room list"
  :group 'matrix-client
  (use-local-map (copy-keymap matrix-client-frame-sidebar-map))
  (local-set-key (kbd "g") #'tabulated-list-revert)
  (local-set-key (kbd "q") #'bury-buffer)
  (local-set-key (kbd "S") #'tabulated-list-sort)
  (setf tabulated-list-format (vector '("U" 1 t) '("🐱" 4 t) '("Name" 25 t) '("Topic" 35 t)
                                      '("Members" 7 matrix-client-room-list-members<)
                                      '("D" 1 t) '("P" 1 t) '("Tags" 15 t) '("Session" 15 t))
        tabulated-list-sort-key '("Name" . nil))
  (add-hook 'tabulated-list-revert-hook #'matrix-client-room-list--set-entries nil 'local)
  (tabulated-list-init-header)
  (matrix-client-room-list--set-entries)
  (tabulated-list-revert))

;;;; Functions

(defun matrix-client-room-list--set-entries ()
  "Set `tabulated-list-entries'."
  ;; Reset avatar size in case default font size has changed.
  (customize-set-variable 'matrix-client-room-avatar-in-buffer-name-size matrix-client-room-avatar-in-buffer-name-size)
  ;; NOTE: From Emacs docs:

  ;; This buffer-local variable specifies the entries displayed in the
  ;; Tabulated List buffer.  Its value should be either a list, or a
  ;; function.
  ;;
  ;; If the value is a list, each list element corresponds to one entry,
  ;; and should have the form ‘(ID CONTENTS)’, where
  ;;
  ;; • ID is either ‘nil’, or a Lisp object that identifies the
  ;; entry.  If the latter, the cursor stays on the same entry when
  ;; re-sorting entries.  Comparison is done with ‘equal’.
  ;;
  ;; • CONTENTS is a vector with the same number of elements as
  ;; ‘tabulated-list-format’.  Each vector element is either a
  ;;  string, which is inserted into the buffer as-is, or a list
  ;;  ‘(LABEL . PROPERTIES)’, which means to insert a text button by
  ;;   calling ‘insert-text-button’ with LABEL and PROPERTIES as
  ;;   arguments (*note Making Buttons::).
  ;;
  ;;   There should be no newlines in any of these strings.
  (setf tabulated-list-entries (->> matrix-client-sessions
                                    (--map (oref it rooms))
                                    -flatten
                                    (-map #'matrix-client-room-list--room-entry))))

(defun matrix-client-room-list--room-entry (room)
  "Return entry for ROOM for `tabulated-list-entries'."
  (with-slots* (((avatar client-data id display-name members session topic) room)
                ((buffer) client-data)
                ((user) session))
    (-let* ((matrix-client-show-room-avatars-in-buffer-names nil)
            (e-unread (if (buffer-modified-p buffer) "U" ""))
            (e-avatar (if avatar (matrix-client-resize-avatar avatar) ""))
            (e-name (list (matrix--room-display-name room) 'buffer buffer 'action #'matrix-client-room-list-action))
            (e-topic (if topic
                         ;; Remove newlines from topic.  Yes, this can happen.
                         (s-replace "\n" " " topic)
                       ""))
            ((e-tags favorite-p low-priority-p) (matrix-client-room-list--tags room))
            (e-direct-p (if (matrix-room-direct-p id session) "D" ""))
            (e-priority (cond (favorite-p "F")
                              (low-priority-p "l")
                              ("N")))
            (e-members (format "%s" (length members))))
      ;; NOTE: We use the room object as the identifying object.  This is allowed, according to `tabulated-list-entries',
      ;; but it uses the object to keep the cursor on the same entry when sorting by comparing with `equal', and I wonder
      ;; if that might be a performance problem in some cases.
      (list room (vector e-unread e-avatar e-name e-topic e-members e-direct-p e-priority e-tags user)))))

(defun matrix-client-room-list--tags (room)
  "Return list (tags-string favorite-p low-priority-p) for ROOM."
  (let (favorite-p low-priority-p)
    (with-slots (tags) room
      (list (->> (cl-loop for (tag-symbol . order) in tags
                          for tag-string = (symbol-name tag-symbol)
                          if (string-prefix-p "u." tag-string)
                          collect (substring tag-string 2)
                          else if (string= tag-string "m.favourite")
                          do (setf favorite-p t)
                          else if (string= tag-string "m.lowpriority")
                          do (setf low-priority-p t)
                          ;; This shouldn't happen.
                          else collect tag-string)
                 (-sort #'string<)
                 (s-join ","))
            favorite-p low-priority-p))))

(defun matrix-client-room-list-members< (a b)
  "Return non-nil if entry A has fewer members than room B.
A and B should be entries from `tabulated-list-mode'."
  (-let (((_room [_unread _avatar _name-for-list _topic a-members _user]) a)
         ((_room [_unread _avatar _name-for-list _topic b-members _user]) b))
    (< (string-to-number a-members) (string-to-number b-members))))

;;;; Footer

(provide 'matrix-client-room-list)

;;; matrix-client-room-list.el ends here
