;;; matrix-notifications.el --- Notifications support  -*- lexical-binding: t; -*-

;;; Commentary:

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'notifications)

(require 'anaphora)

;;;; Customization

(defgroup matrix-client-notifications nil
  "Notifications settings."
  :group 'matrix-client)

(defcustom matrix-client-notifications t
  "Enable notifications."
  ;; This variable may be let-bound to nil to disable notifications, e.g. when loading old messages.
  :type 'boolean)

(defcustom matrix-client-room-notification-rules nil
  "Per-room notification rules.
An alist mapping room IDs to nil (meaning to never notify),
t (meaning to always notify), or a predicate function which takes
two arguments, the room object and the event, and returns non-nil
when a notification should fire."
  :type '(alist :key-type (string :tag "Room ID")
                :value-type (choice (const :tag "Never" nil)
                                    (const :tag "Always" t)
                                    (function :tag "Predicate function"))))

;;;; Variables

(defvar matrix-client-notify-hook nil
  "List of functions called for events.
Each is called with the event-type and the event data.")

(defvar matrix-client-notifications-ring nil
  "Alist of recent notifications mapping notification ID to related buffer.
Automatically trimmed to last 20 notifications.")

(defvar matrix-client-notification-rules
  (a-list "always" t
          "never" nil
          "mention" #'matrix-client-event-mentions-user-p)
  "Alist mapping friendly string names to notification rules.")

;;;; Commands

(matrix-client-def-room-command notify
  :docstring "Set room notification setting.
Without argument, displays help and current setting."
  :insert (pcase input
            ((or 'nil "") (pcase-let* (((eieio client-data id) room)
                                       ((eieio notification-rules) client-data)
                                       (current-rule notification-rules)
                                       (current-rule-name (car (cl-rassoc current-rule matrix-client-notification-rules :test #'equal)))
                                       (available-rules (s-join ", " (-map #'car matrix-client-notification-rules))))
                            (format "Current notification rule: %s.  Available rules: %s." current-rule-name available-rules)))
            (_ (let* ((rule (map-elt matrix-client-notification-rules input nil #'equal))
                      (rule-name (car (cl-rassoc rule matrix-client-notification-rules :test #'equal))))
                 ;; NOTE: If the user enters an invalid rule, it will default to "never".
                 (matrix-client-set-notification-rule room rule)
                 (format "Notification rule set: %s." rule-name)))))

(defun matrix-client-show-notifications-buffer ()
  "Show notifications buffer, if it exists."
  (interactive)
  ;; FIXME: This is messy.
  (switch-to-buffer (oref* (matrix-client--notifications-buffer) client-data buffer)))

;;;; Functions

(defun matrix-client-set-notification-rule (room rule)
  "Set notification RULE for ROOM.
See `matrix-client-notification-rules' for rules."
  (with-slots* (((client-data id) room)
                ((notification-rules) client-data))
    (setq notification-rules rule)
    (map-put matrix-client-room-notification-rules id rule)
    ;; MAYBE: Persist the variable at a different time (or even use a different system for
    ;; persistence), because calling `customize-save-variable' is a bit slow.
    (customize-save-variable 'matrix-client-room-notification-rules matrix-client-room-notification-rules)))

(defun matrix-client-notify (event-type data &rest rest)
  "Run notify hooks and built-in notificataion for an event of EVENT-TYPE with DATA.
Optional REST of args are also applied to hooks and function."
  ;; FIXME: Pass session so we can get its initial-sync-p.  Probably should be a method specialized
  ;; on session.
  (when matrix-client-notifications
    (unless (oref (car matrix-client-sessions) initial-sync-p)
      (run-hook-with-args 'matrix-client-notify-hook event-type data rest)
      ;; Run built-in notification for this event type
      ;; TODO: Use `if-fn-apply' here.
      (let ((fn (intern-soft (concat "matrix-client-notify-" event-type))))
        (when (functionp fn)
          (apply #'funcall fn data rest))))))

(defun matrix-client-notify--add-to-notifications-buffer (event-type data rest)
  "Add event with DATA to notifications buffer."
  ;; FIXME: Handle other event types.
  (pcase event-type
    ("m.room.message"
     (let-alist data
       ;; FIXME: Should probably rework a lot of code in this file to pass event arguments in a better way.
       (let* ((room (plist-get rest :room))
              (room-name (propertize (matrix-client-display-name room)
                                     'face 'font-lock-function-name-face))
              (room-buffer (oref* room client-data buffer))
              (sender (propertize (concat (matrix-user-displayname room .sender) ">")
                                  'face 'font-lock-keyword-face))
              (body .content.body)
              (event-id .event_id)
              ;; (blank (propertize " " 'face 'matrix-client-metadata))
              ;; The space is required because of an idiosyncrasy with
              ;; how Emacs handles images in display properties.
              ;; Without it, only the room avatar is visible, and it's
              ;; doubled, and there is no other text visible.
              (message (propertize (format$ " $room-name: $sender $body")
                                   'buffer room-buffer
                                   'event_id event-id)))
         ;; Using notification buffer pseudo room
         (matrix-client-insert (matrix-client--notifications-buffer) message
                               :timestamp-prefix t))))))

(add-hook 'matrix-client-notify-hook #'matrix-client-notify--add-to-notifications-buffer)

(defun matrix-client--notifications-buffer ()
  "Return `matrix-room' object whose buffer is the special notifications buffer.
This function exists to allow the use of `with-room-buffer'."
  (let ((buffer (or (get-buffer "*Matrix Notifications*")
                    (aprog1 (get-buffer-create "*Matrix Notifications*")
                      (with-current-buffer it
                        (use-local-map (make-sparse-keymap))
                        (local-set-key (kbd "RET") #'matrix-client-notifications-buffer-pop)
                        (matrix-client-insert-prompt)
                        (matrix-client-insert-last-seen))))))
    (matrix-room :client-data (matrix-room-client-data :buffer buffer))))

(defun matrix-client-notifications-buffer-pop ()
  "Pop to room buffer for event at point."
  (interactive)
  (let* ((properties (text-properties-at (point)))
         (buffer (lax-plist-get properties 'buffer))
         (event-id (lax-plist-get properties 'event_id)))
    (when buffer
      (pop-to-buffer buffer)
      (when event-id
        (awhen (matrix-client--find-propertized-string (list 'event_id event-id))
          (goto-char (car it)))))))

;; MAYBE: Use a macro to define the handlers, because they have to
;; define their arg lists in a certain way, and the macro would take
;; care of that automatically.

(cl-defun matrix-client-notify-m.room.message (event &key room &allow-other-keys)
  "Show notification for m.room.message events.
EVENT should be the `event' variable from the
`defmatrix-client-handler'.  ROOM should be the room object."
  ;; NOTE: Adding notification rule suport
  (when (matrix-client-notify-p room event)
    (pcase-let* (((map content sender event_id) event)
                 ((map body) content)
                 ((eieio client-data) room)
                 ((eieio buffer) client-data)
                 (display-name (matrix-user-displayname room sender))
                 (id (notifications-notify :title (format$ "<b>$display-name</b>")
                                           ;; Encode the message as ASCII because dbus-notify
                                           ;; can't handle some Unicode chars.  And
                                           ;; `ignore-errors' doesn't work to ignore errors
                                           ;; from it.  Don't ask me why.
                                           :body (encode-coding-string body 'us-ascii)
                                           :category "im.received"
                                           :timeout 5000
                                           :app-icon nil
                                           :actions '("default" "Show")
                                           :on-action #'matrix-client-notification-show)))
      (map-put matrix-client-notifications-ring id (a-list 'buffer buffer
                                                           'event_id event_id))
      ;; Trim the list
      (setq matrix-client-notifications-ring (-take 20 matrix-client-notifications-ring)))))

(defun matrix-client-notification-show (id key)
  "Show the buffer for a notification.
This function is called by `notifications-notify' when the user
activates a notification."
  (pcase-let* (((map (id notification)) matrix-client-notifications-ring)
               ((map buffer event_id) notification)
               (window (get-buffer-window buffer)))
    (raise-frame)
    (if window
        (select-window window)
      (switch-to-buffer buffer))
    ;; Go to relevant event
    (or (cl-loop initially do (goto-char (point-max))
                 for value = (get-text-property (point) 'event_id)
                 thereis (equal value event_id)
                 while (when-let (pos (previous-single-property-change (point) 'event_id))
                         (goto-char pos)))
        ;; Not found: go to last line
        (goto-char (point-max)))))

;;;;; Predicates

(defun matrix-client-notify-p (room event)
  "Return non-nil if notification should be displayed for EVENT in ROOM."
  ;; FIXME: Initialize to a user-defined default.
  (pcase (oref* room client-data notification-rules)
    ((and fn (pred functionp)) (funcall fn room event))
    ('nil nil)
    ('t t)))

(defun matrix-client-event-mentions-user-p (room event)
  "Return non-nil if EVENT mentions logged-in user in ROOM.
Uses user's displayname in ROOM."
  (pcase-let* (((map content) event)
               ((map body) content)
               ((eieio session) room)
               ((eieio user) session)
               (displayname (matrix-user-displayname room user))
               (regexp (rx-to-string `(seq (or ,user ,displayname)))))
    (string-match-p regexp body)))

;; MAYBE: Define these rules with a macro.

;;;; Footer

(provide 'matrix-notifications)

;;; matrix-notifications.el ends here
