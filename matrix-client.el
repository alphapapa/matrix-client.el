;;; matrix-client.el --- A minimal chat client for the Matrix.org RPC

;; Copyright (C) 2015 Ryan Rix
;; Author: Ryan Rix <ryan@whatthefuck.computer>
;; Maintainer: Ryan Rix <ryan@whatthefuck.computer>
;; Created: 21 June 2015
;; Keywords: web
;; Homepage: http://doc.rix.si/matrix.html
;; Package-Version: 0.1.2
;; Package-Requires: ((emacs "25.1") (json "1.4") (request "0.2.0") (a "0.1.0"))

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
(require 'cl-lib)

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

;;;###autoload
(defclass matrix-client-connection (matrix-connection)
  ((running :initarg :running
            :initform nil
            :documentation "BOOL specifiying if the event listener is currently running.")
   (rooms :initarg :rooms
          :initform nil
          :documentation "List of matrix-room objects")
   (end-token :initarg :end-token)
   (event-handlers :initarg :event-handlers
                   :documentation "An alist of (type . function) handler definitions for various matrix types.

Each of these receives the raw event as a single DATA argument.
See `defmatrix-client-handler'.")
   (event-hook :initarg :event-hook
               :documentation "A lists of functions that are evaluated when a new event comes in.")
   (username :initarg :username
             :documentation "Your Matrix username.")
   (input-filters :initarg :input-filters
                  :documentation "List of functions to run input through.

Each of these functions take a single argument, the TEXT the user
inputs.  They can modify that text and return a new version of
it, or they can return nil to prevent further processing of it.")
   (watchdog-timer :initarg :watchdog-timer)
   (last-event-ts :initarg :last-event-ts))
  :documentation "This is the basic UI encapsulation of a Matrix connection.

To build a UI on top of `matrix-api' start here, wire up
event-handlers and input-filters.")

(defvar-local matrix-client-room-connection nil
  "`matrix-client-connection' object for the current buffer")

(defvar-local matrix-client-room-object nil
  "`matrix-client-room' object for the current buffer")

;; (defvar matrix-client-event-stream-end-token nil)

(defclass matrix-client-room ()
  ((con :initarg :con)
   (buffer :initarg :buffer
           :documentation "The buffer that contains the room's chat session")
   (name :initarg :room-name
         :documentation "The name of the buffer's room.")
   (aliases :initarg :aliases
            :documentation "The alises of the buffer's room.")
   (topic :initarg :topic
          :documentation "The topic of the buffer's room.")
   (id :initarg :id
       :documentation "The Matrix ID of the buffer's room.")
   (typers :initarg :typers)
   (membership :initarg :membership
               :documentation "The list of members of the buffer's room.")
   (end-token :initarg :end-token
              :documentation "The most recent event-id in a room, used to push read-receipts to the server.")))

(defvar-local matrix-client-room-typers nil
  "The list of members of the buffer's room who are currently typing.")

(defvar matrix-client-connections '()
  "Alist of (username . connection)")

(defvar matrix-client-after-connect-hooks nil
  "A list of functions to run when a new Matrix Client connection occurs.")

(require 'matrix-client-handlers)
(require 'matrix-client-modes)

;;;###autoload
(defun matrix-client (username)
  "Connect to Matrix."
  (interactive "i")
  (let* ((base-url matrix-homeserver-base-url)
         ;; Pass a username in to get an existing connection
         (con (when username
                (matrix-get username matrix-client-connections)))
         (con (unless con
                (matrix-client-connection
                 matrix-homeserver-base-url
                 :base-url matrix-homeserver-base-url))))
    (unless (and (slot-boundp con :token) (oref con :token))
      (matrix-client-login con username))
    (unless (oref con :running)
      (matrix-client-start-watchdog con nil 120)
      (matrix-client-inject-event-listeners con)
      (matrix-client-handlers-init con)
      (matrix-sync con nil t matrix-client-event-poll-timeout
                   (apply-partially #'matrix-client-sync-handler con))
      (dolist (hook matrix-client-after-connect-hooks)
        (funcall hook con)))
    (add-to-list 'matrix-client-connections (list (oref con :username) con))
    (oset con :running t)
    (message "You're jacked in, welcome to Matrix. Your messages will arrive momentarily.")))

(defmethod matrix-client-login ((con matrix-client-connection) &optional username)
  "Get a token form the Matrix homeserver.

If [`matrix-client-use-auth-source'] is non-nil, attempt to log in
using data from auth-source. Otherwise, the user will be prompted
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

(defun matrix-client-disconnect (&optional con)
  "Disconnect from Matrix and kill all active room buffers."
  (interactive)
  (let ((discon (lambda (con)
                  (dolist (room (oref (cadr con) :rooms))
                    (kill-buffer (oref (cdr room) :buffer)))
                  (oset (cadr con) :running nil)
                  (setq matrix-client-connections
                        (cl-remove (car con)
                                   matrix-client-connections
                                   :test 'equal
                                   :key 'car)))))
    (if con
        (funcall discon con)
      (dolist (con matrix-client-connections)
        (funcall discon con)))))

(defmethod matrix-client-start-watchdog ((con matrix-client-connection) &optional force timer-secs)
  (when (or force matrix-client-enable-watchdog)
    (let ((last-ts (or (and (slot-boundp con :last-event-ts)
                            (oref con :last-event-ts))
                       0))
          (next (and (slot-boundp con :end-token)
                     (oref con :end-token)))
          (timer (and (slot-boundp con :watchdog-timer)
                      (oref con :watchdog-timer))))
      (if (and (slot-boundp con :watchdog-timer) ;; start timer if not running
               (oref con :watchdog-timer))
          (if (> (* 1000 (- (float-time) last-ts)) ;; If we've timed out, re-sync
                 matrix-client-event-poll-timeout)
              (progn
                (cancel-timer timer)
                ;; XXX Pull these fucking syncs out and bar them on (oref con :running)
                (when (and (slot-boundp con :running)
                           (oref con :running))
                  (message "Reconnecting you to Matrix, one monent please.")
                  (cancel-timer timer)
                  (matrix-sync con next nil matrix-client-event-poll-timeout
                               (apply-partially #'matrix-client-sync-handler con))))
            (cancel-timer timer)))
      (oset con :watchdog-timer
            (run-with-timer (or timer-secs (/ (* 2 matrix-client-event-poll-timeout) 1000))
                            (or timer-secs (/ (* 2 matrix-client-event-poll-timeout) 1000))
                            (apply-partially #'matrix-client-start-watchdog con))))))

(defmethod matrix-client-setup-room ((con matrix-client-connection) room-id)
  (when (get-buffer room-id)
    (kill-buffer room-id))
  (let* ((room-buf (get-buffer-create room-id))
         (room-obj (matrix-client-room room-id :buffer room-buf :con con))
         (new-room-list (append (oref con :rooms) (list (cons room-id room-obj)))))
    (with-current-buffer room-buf
      (matrix-client-mode)
      (erase-buffer)
      (matrix-client-render-message-line room-obj))
    (switch-to-buffer room-buf)
    (set (make-local-variable 'matrix-client-room-connection) con)
    (set (make-local-variable 'matrix-client-room-object) room-obj)
    (oset con :rooms new-room-list)
    (oset room-obj :id room-id)
    (oset room-obj :buffer room-buf)
    room-obj))

(defmethod matrix-client-sync-handler ((con matrix-client-connection) data)
  (when (oref con :running)
    (mapc
     (lambda (room-data)
       (let* ((room-id (symbol-name (car room-data)))
              (room (matrix-client-room-for-id con room-id)))
         (when (and room (slot-boundp room :buffer))
           (kill-buffer (oref room :buffer)))))
     (matrix-get 'leave (matrix-get 'rooms data)))
    (mapc
     (lambda (room-data)
       (let* ((room-id (symbol-name (car room-data)))
              (room (or (matrix-get room-id (oref con :rooms))
                        (matrix-client-setup-room con room-id)))
              (room-events (cdr room-data)))
         (mapc
          (lambda (event)
            (matrix-client-room-event room event))
          (matrix-get 'events (matrix-get 'state room-events)))
         (mapc
          (lambda (event)
            (matrix-client-room-event room event))
          (matrix-get 'events (matrix-get 'timeline room-events)))))
     (matrix-get 'join (matrix-get 'rooms data)))
    (mapc
     (lambda (room-data)
       (matrix-client-invite-room con data))
     (matrix-get 'invite (matrix-get 'rooms data)))
    (let ((next (matrix-get 'next_batch data)))
      (oset con :end-token next)
      (oset con :last-event-ts  (float-time))
      (matrix-client-start-watchdog con)
      (matrix-sync con next nil matrix-client-event-poll-timeout
                   (apply-partially #'matrix-client-sync-handler con)))))

(defmethod matrix-client-room-event ((room matrix-client-room) event)
  "Handle state events from a sync."
  (let* ((con (oref room :con)))
    (when (slot-boundp con :event-hook)
      (mapc (lambda (hook)
              (funcall hook con room event))
            (oref con :event-hook)))))

(defmethod matrix-client-render-event-to-room ((con matrix-client-connection) room item)
  "Feed ITEM in to its proper `matrix-client-event-handlers' handler."
  (let* ((type (matrix-get 'type item))
         (handler (matrix-get type (oref con :event-handlers))))
    (when handler
      (funcall handler con room item))))

(defmethod matrix-client-inject-event-listeners ((con matrix-client-connection))
  "Inject the standard event listeners."
  (unless (slot-boundp con :event-hook)
    (oset con :event-hook
          '(matrix-client-debug-event-maybe
            matrix-client-render-event-to-room))))

(defmethod matrix-client-debug-event-maybe ((con matrix-client-connection) room data)
  "Debug DATA to *matrix-events* if `matrix-client-debug-events' is non-nil."
  (with-current-buffer (get-buffer-create "*matrix-events*")
    (let ((inhibit-read-only t))
      (when matrix-client-debug-events
        (goto-char (point-max))
        (insert "\n")
        (insert (prin1-to-string data))))))

(defun matrix-client-update-header-line (room)
  "Update the header line of the current buffer."
	;; Disable when tabbar mode is on
	(unless (and (boundp 'tabbar-mode) tabbar-mode)
		(let ((typers (and (slot-boundp room :typers)
										(oref room :typers)))
					 (name (and (slot-boundp room :room-name)
                   (oref room :room-name)))
					 (topic (and (slot-boundp room :topic)
                    (oref room :topic))))
			(if (> 0 (length typers))
        (progn
          (setq header-line-format (format "(%d typing...) %s: %s" (length typers) name topic)))
				(setq header-line-format (format "%s: %s" name topic))))))

;;;###autoload
(defmacro insert-read-only (text &rest extra-props)
  "Insert a block of TEXT as read-only, with the ability to add EXTRA-PROPS such as face."
  `(add-text-properties
    (point) (progn
              (insert ,text)
              (point))
    '(read-only t ,@extra-props)))

(defmethod matrix-client-render-message-line ((room matrix-client-room room))
  "Insert a message input at the end of the buffer."
  (goto-char (point-max))
  (let ((inhibit-read-only t))
    (insert "\n")
    (insert-read-only "[::] ▶ " rear-nonsticky t)
    (setq buffer-undo-list nil)))

(defun matrix-client-send-active-line ()
  "Send the current message-line text after running it through input-filters."
  (interactive)
  (let ((buffer-undo-list t))
    (goto-char (point-max))
    (beginning-of-line)
    (re-search-forward "▶")
    (forward-char)
    (kill-line)
    (let* ((room matrix-client-room-object)
           (con (and (slot-boundp room :con)
                     (oref room :con)))
           (input-filters (and (slot-boundp con :input-filters)
                               (oref con :input-filters))))
      (cl-reduce 'matrix-client-run-through-input-filter
                 input-filters
                 :initial-value (pop kill-ring)))))

(defun matrix-client-run-through-input-filter (text filter)
  "Run each TEXT through a single FILTER.  Used by `matrix-client-send-active-line'."
  (let ((con (oref matrix-client-room-object :con)))
    (when text
      (funcall filter con text))))

(defun matrix-client-send-to-current-room (con text)
  "Send a string TEXT to the current buffer's room."
  (let ((room matrix-client-room-object)
        (con (and room
                  (slot-boundp room :con)
                  (oref room :con)))
        (id (and room
                 (slot-boundp room :id)
                 (oref room :id))))
    (matrix-send-message con id text))
  text)

(defun matrix-client-window-change-hook ()
  "Send a read receipt if necessary."
  ;; (when (and matrix-client-room-id matrix-client-room-end-token)
  ;;   (message "%s as read from %s" matrix-client-room-end-token matrix-client-room-id)
  ;;   (matrix-mark-as-read matrix-client-room-id matrix-client-room-end-token))
  )

(provide 'matrix-client)
;;; matrix-client.el ends here
