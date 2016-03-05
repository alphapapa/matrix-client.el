;;; matrix-client.el --- A minimal chat client for the Matrix.org RPC

;; Copyright (C) 2015 Ryan Rix
;; Author: Ryan Rix <ryan@whatthefuck.computer>
;; Maintainer: Ryan Rix <ryan@whatthefuck.computer>
;; Created: 21 June 2015
;; Keywords: web
;; Homepage: http://doc.rix.si/matrix.html
;; Package-Version: 0.1.2
;; Package-Requires: ((json "1.4") (request "0.2.0"))

;; This file is not part of GNU Emacs.

;; matrix-client.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.
;;
;; matrix-client.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `matrix-client' is a chat client and API library for the Matrix.org decentralized RPC
;; system. `(package-install 'matrix-client)' and then either deploy your own homeserver or register
;; an account on the public homeserver https://matrix.org/beta/#/login . After you've done that, M-x
;; matrix-client will set you up with buffers corresponding to your Matrix rooms. You can join new
;; ones with /join, leave with /leave or /part, and hook in to the custom functions provided by
;; =matrix-client=.

;; Implementation-wise `matrix-client' itself provides most of the core plumbing for
;; an interactive Matrix chat client. It uses the Matrix event stream framework
;; to dispatch a global event stream to individual rooms. There are a set of
;; 'event handlers' and 'input filters' in `matrix-client-handlers' which are used to
;; implement the render flow of the various event types and actions a user can
;; take.

;;; Code:

(require 'matrix-api)
(require 'matrix-client-handlers)
(require 'matrix-client-modes)

;;;###autoload
(defcustom matrix-client-debug-events nil
  "When non-nil, log raw events to *matrix-events* buffer."
  :type 'boolean
  :group 'matrix-client)

;;;###autoload
(defcustom matrix-client-event-poll-timeout 30000
  "How long to wait for a Matrix event in the EventStream before timing out and trying again."
  :type 'number
  :group 'matrix-client)

;;;###autoload
(defcustom matrix-client-backfill-count 10
  "How many messages to backfill at a time when scrolling."
  :group 'matrix-client)

;;;###autoload
(defcustom matrix-client-backfill-threshold 5
  "How close to the top of a buffer point needs to be before backfilling events."
  :group 'matrix-client)

;;;###autoload
(defcustom matrix-client-render-presence t
  "Show presence changes in the main buffer windows."
  :group 'matrix-client)

;;;###autoload
(defcustom matrix-client-render-membership t
  "Show membership changes in the main buffer windows."
  :group 'matrix-client)

;;;###autoload
(defcustom matrix-client-render-html (featurep 'shr)
  "Render HTML messages in buffers. These are currently the
ad-hoc 'org.matrix.custom.html' messages that Vector emits."
  :group 'matrix-client)

;;;###autoload
(defcustom matrix-client-enable-watchdog t
  "If enabled, a timer will be run after twice the interval of
`matrix-client-event-poll-timeout'."
  :group 'matrix-client)

(defvar matrix-client-event-handlers '()
  "An alist of (type . function) handler definitions for various matrix types.

Each of these receives the raw event as a single DATA argument.
See `defmatrix-client-handler'. This value is used as the default
for every `matrix-client-connection' and can be overridden on a
connection basis.")

;; (defvar matrix-client-active-rooms nil
;;   "Rooms the active client is in.")

;; (defvar matrix-client-event-listener-running nil)

;; (defvar matrix-client-new-event-hook nil
;;   )

(defclass matrix-client-connection (matrix-connection)
  ((running :initarg :running
            :initform nil
            :documentation "BOOL specifiying if the event listener is currently running.")
   (rooms :initarg :rooms
          :initform nil
          :documentation "List of matrix-room objects")
   (event-handlers :initarg :event-handlers
                   :documentation "An alist of (type . function) handler definitions for various matrix types.

Each of these receives the raw event as a single DATA argument.
See `defmatrix-client-handler'.")
   (event-hook :initarg :event-hook
               :documentation "A lists of functions that are evaluated when a new event comes in.")
   (username :initarg :username
             :documentation "Your Matrix username.")
   (end-token)
   (input-filters :initarg :input-filters
                  :documentation "List of functions to run input through.

Each of these functions take a single argument, the TEXT the user
inputs.  They can modify that text and return a new version of
it, or they can return nil to prevent further processing of it."))
  :documentation "This is the basic UI encapsulation of a Matrix connection.

To build a UI on top of `matrix-api' start here, wire up
event-handlers and input-filters.")
(oset-default matrix-client-connection event-handlers matrix-client-event-handlers)

;; (defvar matrix-username nil
;;   "Your Matrix username.")

;; (defvar matrix-client-event-stream-end-token nil)

;; (defvar matrix-client-input-filters nil
;;   )

;; (defvar-local matrix-client-room-name nil
;;   )

(defclass matrix-client-room ()
  ((buffer :initarg :buffer
           :documentation "The buffer that contains the room's chat session")
   (name :initarg :room-name
         :documentation "The name of the buffer's room.")
   (aliases :initarg :aliases
            :documentation "The alises of the buffer's room.") 
   (topic :initarg :topic
          :documentation "The topic of the buffer's room.")
   (id :initarg :id
       :documentation "The Matrix ID of the buffer's room.")
   (membership :initarg :membership
               :documentation "The list of members of the buffer's room.")
   (end-token :init-arg :end-token
              :documentation "The most recent event-id in a room, used to push read-receipts to the server.")))


;; (defvar-local matrix-client-room-aliases nil
;;   )
;; (defvar-local matrix-client-room-topic nil
;;   )
;; (defvar-local matrix-client-room-id nil
;;   )
;; (defvar-local matrix-client-room-membership nil
;;   )
;; (defvar-local matrix-client-room-end-token nil
;;   )

(defvar-local matrix-client-room-typers nil
  "The list of members of the buffer's room who are currently typing.")

(defvar matrix-client-connections '()
  "Alist of (username . connection)")

(defvar matrix-client-watchdog-last-message-ts nil
  "Number of seconds since epoch of the last message.

Used in the watchdog timer to fire a reconnect attempt.")

(defvar matrix-client-watchdog-timer nil)

;;;###autoload
(defun matrix-client (username)
  "Connect to Matrix."
  (interactive "i")
  (let* ((base-url matrix-homeserver-base-url)
         (con (if username ;; Pass a username in to get an existing connection
                  (matrix-get username matrix-client-connections)
                (matrix-client-connection
                 matrix-homeserver-base-url
                 :base-url matrix-homeserver-base-url))))
    (unless (and (slot-boundp con :token) (oref con :token))
      (matrix-client-login con))
    (unless (oref con :running)
      (matrix-client-inject-event-listeners con)
      (matrix-client-handlers-init con))
    (add-to-list 'matrix-client-connections (cons (oref con :username) con))
    (message "You're jacked in, welcome to Matrix. Your messages will arrive momentarily.")
    (matrix-sync con nil t matrix-client-event-poll-timeout
                 (apply-partially #'matrix-client-sync-handler con))
    ;; (let* ((initial-data (matrix-initial-sync con 25)))
    ;;   (mapc 'matrix-client-set-up-room (matrix-get 'rooms initial-data))
    ;;   
    ;;   (when matrix-client-enable-watchdog
    ;;     (matrix-client-setup-watchdog-timer))
    ;;   (setq matrix-client-event-listener-running t)
    ;;   (matrix-client-start-event-listener (matrix-get 'end initial-data)))
    ))

(defmethod matrix-client-sync-handler ((con matrix-client-connection) data)
  (mapc
   (lambda (room-data)
     (matrix-client-leave-room con room-data))
   (matrix-get 'leave (matrix-get 'rooms data)))
  (mapc
   (lambda (room-data)
     (let ((room-id (symbol-name (car room-data)))
           (room-events (cdr room-data)))
       (mapc
        (lambda (event)
          (matrix-client-room-event con room-id event))
        (matrix-get 'events (matrix-get 'state room-events)))
       (mapc
        (lambda (event)
          (matrix-client-room-event con room-id event))
        (matrix-get 'events (matrix-get 'timeline room-events)))))
   (matrix-get 'join (matrix-get 'rooms data)))
  (mapc
   (lambda (room-data)
     (matrix-client-invite-room con data))
   (matrix-get 'invite (matrix-get 'rooms data))))

;;;###autoload
(defmethod matrix-client-login ((con matrix-client-connection) &optional username)
  "Get a token form the Matrix homeserver.

If [`matrix-client-use-auth-source'] is non-nil, attempt to log in
using data from auth-source.  Otherwise, the user will be prompted
for a username and password."
  (let* ((auth-source-creation-prompts
          '((username . "Matrix identity: ")
            (secret . "Matrix password for %u (homeserver: %h): ")))
         (found (nth 0 (auth-source-search :max 1
                                           :host (oref con :base-url)
                                           :user username
                                           :require '(:user :secret)
                                           :create t))))
    (when (and
           found
           (matrix-login-with-password con
                                       (plist-get found :user)
                                       (let ((secret (plist-get found :secret)))
                                         (if (functionp secret)
                                             (funcall secret)
                                           secret)))
           (oset con :username (plist-get found :user))
           (let ((save-func (plist-get found :save-function)))
             (when save-func (funcall save-func)))))))

(defmethod matrix-client-setup-room ((con matrix-client-connection) room-id)
  (when (get-buffer room-id)
    (kill-buffer room-id))
  (let* ((room-buf (get-buffer-create room-id))
         (room-obj (matrix-client-room room-id :buffer room-buf))
         (new-room-list (append (oref con :rooms) (list (cons room-id room-obj)))))
    (with-current-buffer room-buf
      (matrix-client-mode)
      (erase-buffer)
      (matrix-client-render-message-line))
    (switch-to-buffer room-buf)
    (oset con :rooms new-room-list)
    room-obj))

(defmethod matrix-client-room-event ((con matrix-client-connection) room-id event)
  "Handle state events from a sync."
  (let* ((room (or (matrix-get room-id (oref con :rooms))
                   (matrix-client-setup-room con room-id))))
    (matrix-client-render-event-to-room con room event)))

(defmethod matrix-client-render-event-to-room ((con matrix-client-connection) room item)
  "Feed ITEM in to its proper `matrix-client-event-handlers' handler."
  (let* ((type (matrix-get 'type item))
         (handler (matrix-get type matrix-client-event-handlers)))
    (when handler
      (funcall handler con room item))))

(defmethod matrix-client-inject-event-listeners ((con matrix-client-connection))
  "Inject the standard event listeners."
  (unless (slot-boundp con :event-hook)
    (oset con :event-hook
          '(matrix-client-debug-event-maybe
            matrix-client-render-events-to-room
            matrix-client-set-room-end-token))))

(defun matrix-client-debug-event-maybe (data)
  "Debug DATA to *matrix-events* if `matrix-client-debug-events' is non-nil."
  (with-current-buffer (get-buffer-create "*matrix-events*")
    (let ((inhibit-read-only t))
      (when matrix-client-debug-events
        (end-of-buffer)
        (insert "\n")
        (insert (prin1-to-string data))))))

(defun matrix-client-update-header-line ()
  "Update the header line of the current buffer."
  (if (> 0 (length matrix-client-room-typers))
      (progn
        (setq header-line-format (format "(%d typing...) %s: %s" (length matrix-client-room-typers)
                                         matrix-client-room-name matrix-client-room-topic)))
    (setq header-line-format (format "%s: %s" matrix-client-room-name matrix-client-room-topic))))

;;;###autoload
(defmacro insert-read-only (text &rest extra-props)
  "Insert a block of TEXT as read-only, with the ability to add EXTRA-PROPS such as face."
  `(add-text-properties
    (point) (progn
              (insert ,text)
              (point))
    '(read-only t ,@extra-props)))

(defun matrix-client-render-message-line ()
  "Insert a message input at the end of the buffer."
  (end-of-buffer)
  (let ((inhibit-read-only t))
    (insert "\n")
    (insert-read-only (format "🔥 [%s] ▶ " matrix-client-room-id) rear-nonsticky t)))

(provide 'matrix-client)
;;; matrix-client.el ends here
