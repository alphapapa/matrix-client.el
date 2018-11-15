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
An alist mapping room IDs to lists of predicate functions which
take a single argument, the event.  When a function returns
non-nil, the notification should be displayed."
  :type '(alist :key-type string
                :value-type (choice (function)
                                    (repeat function))))

;;;; Variables

(defvar matrix-client-notify-hook nil
  "List of functions called for events.
Each is called with the event-type and the event data.")

(defvar matrix-client-notifications-ring nil
  "Alist of recent notifications mapping notification ID to related buffer.
Automatically trimmed to last 20 notifications.")

(defvar matrix-client-notification-rules
  (a-list "always" #'identity
          "never" #'ignore
          "mention" #'matrix-client-event-mentions-user-p)
  "Alist mapping friendly string names to notification rule functions.")

;;;; Commands

(matrix-client-def-room-command notify
  :docstring "Set room notification setting.
Without argument, displays help and current setting."
  :insert (pcase input
            ("" (pcase-let* (((eieio client-data id) room)
                             ((eieio notification-rules) client-data)
                             ;; NOTE: `nil' is handled as "always"/`identity' for now.
                             (current-rule (or notification-rules #'identity))
                             (current-rule-name (car (cl-rassoc current-rule matrix-client-notification-rules :test #'equal)))
                             (available-rules (s-join ", " (-map #'car matrix-client-notification-rules))))
                  (format "Current notification rule: %s.  Available rules: %s." current-rule-name available-rules)))
            (_ (let ((rule (map-elt matrix-client-notification-rules input nil #'equal)))
                 (unless rule
                   (user-error "Invalid notification rule: %s" input))
                 (matrix-client-set-notification-rule room rule)
                 (format "Notification rule set: %s" input)))))

;;;; Functions

(cl-defmethod matrix-client-set-notification-rule ((room matrix-room) fn)
  "Set notification rule for ROOM to the predicate FN.
e.g. `ignore' disables notifications for the room."
  (with-slots* (((client-data id) room)
                ((notification-rules) client-data))
    (setq notification-rules fn)
    ;; FIXME: Does this properly save the new customization setting?  I don't think it does...
    (map-put matrix-client-room-notification-rules id fn)))

(defun matrix-client-notify (event-type data &rest rest)
  "Run notify hooks and built-in notificataion for an event of EVENT-TYPE with DATA.
Optional REST of args are also applied to hooks and function."
  ;; FIXME: Pass session so we can get its initial-sync-p.  Probably should be a method specialized
  ;; on session.
  (when matrix-client-notifications
    (unless (oref (car matrix-client-sessions) initial-sync-p)
      (run-hook-with-args 'matrix-client-notify-hook event-type data rest)
      ;; Run built-in notification for this event type
      (let ((fn (intern-soft (concat "matrix-client-notify-" event-type))))
        (when (functionp fn)
          (apply #'funcall fn data rest))))))

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

(cl-defmethod matrix-client-notify-p ((room matrix-room) event)
  "Return non-nil if notification should be displayed for EVENT in ROOM."
  (pcase-let* (((eieio client-data) room)
               ((eieio buffer notification-rules) client-data))
    (or (null notification-rules)
        (cl-loop for pred in (cl-typecase notification-rules
                               (list notification-rules)
                               (function (list notification-rules)))
                 always (funcall pred room event)))))

(cl-defmethod matrix-client-event-mentions-user-p ((room matrix-room) event)
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
