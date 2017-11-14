;;; matrix-client.el --- A minimal chat client for the Matrix.org RPC

;; Copyright (C) 2015 Ryan Rix
;; Author: Ryan Rix <ryan@whatthefuck.computer>
;; Maintainer: Ryan Rix <ryan@whatthefuck.computer>
;; Created: 21 June 2015
;; Keywords: web
;; Homepage: http://doc.rix.si/matrix.html
;; Package-Version: 0.1.2
;; Package-Requires: ((emacs "25.1") (dash "2.13.0") (json "1.4") (request "0.2.0") (a "0.1.0"))

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
(require 'seq)

(require 'dash)

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
   (end-token :initarg :end-token
              :initform nil)
   (event-handlers :initarg :event-handlers
                   :initform nil
                   :documentation "An alist of (type . function) handler definitions for various matrix types.

Each of these receives the raw event as a single DATA argument.
See `defmatrix-client-handler'.")
   (event-hook :initarg :event-hook
               :initform nil
               :documentation "A lists of functions that are evaluated when a new event comes in.")
   (username :initarg :username
             :initform nil
             :documentation "Your Matrix username.")
   (input-filters :initarg :input-filters
                  :initform nil
                  :documentation "List of functions to run input through.

Each of these functions take a single argument, the TEXT the user
inputs.  They can modify that text and return a new version of
it, or they can return nil to prevent further processing of it.")
   (watchdog-timer :initarg :watchdog-timer
                   :initform nil)
   (last-event-ts :initarg :last-event-ts
                  :initform 0))
  :documentation "This is the basic UI encapsulation of a Matrix connection.

To build a UI on top of `matrix-api' start here, wire up
event-handlers and input-filters.")

(defvar-local matrix-client-room-connection nil
  "`matrix-client-connection' object for the current buffer")

(defvar-local matrix-client-room-object nil
  "`matrix-client-room' object for the current buffer")

;; (defvar matrix-client-event-stream-end-token nil)

(defclass matrix-client-room ()
  ((con :initarg :con
        :initform nil)
   (buffer :initarg :buffer
           :initform nil
           :documentation "The buffer that contains the room's chat session")
   (name :initarg :room-name
         :initform nil
         :documentation "The name of the buffer's room.")
   (aliases :initarg :aliases
            :initform nil
            :documentation "The aliases of the buffer's room.")
   (topic :initarg :topic
          :initform nil
          :documentation "The topic of the buffer's room.")
   (id :initarg :id
       :initform nil
       :documentation "The Matrix ID of the buffer's room.")
   (typers :initarg :typers
           :initform nil)
   (membership :initarg :membership
               :initform nil
               :documentation "The list of members of the buffer's room.")
   (end-token :initarg :end-token
              :initform nil
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
         (con (if username
                  (map-elt matrix-client-connections username)
                (matrix-client-connection matrix-homeserver-base-url
                                          :base-url matrix-homeserver-base-url))))
    (unless (oref con :token)
      (matrix-client-login con username))
    (unless (oref con :running)
      (matrix-client-start-watchdog con nil 120)
      (matrix-client-inject-event-listeners con)
      (matrix-client-handlers-init con)
      (matrix-sync con nil t matrix-client-event-poll-timeout
                   (apply-partially #'matrix-client-sync-handler con))
      (run-hook-with-args 'matrix-client-after-connect-hooks con))
    (map-put matrix-client-connections (oref con :username) con)
    (oset con :running t)
    (message "You're jacked in, welcome to Matrix. Your messages will arrive momentarily.")))

(cl-defmethod matrix-client-login ((con matrix-client-connection) &optional username)
  "Login to Matrix connection CON and get a token.
If [`matrix-client-use-auth-source'] is non-nil, attempt to log
in using data from auth-source.  Otherwise, prompt for username
and password."
  (let* ((auth-source-creation-prompts (a-list 'username "Matrix identity: "
                                               'secret "Matrix password for %u (homeserver: %h): "))
         (found (nth 0 (auth-source-search :max 1
                                           :host (oref con :base-url)
                                           :user username
                                           :require '(:user :secret)
                                           :create t))))
    (when (and found
               (matrix-login-with-password con (plist-get found :user)
                                           (let ((secret (plist-get found :secret)))
                                             (if (functionp secret)
                                                 (funcall secret)
                                               secret))))
      (oset con :username (plist-get found :user))
      (when-let ((save-func (plist-get found :save-function)))
        (funcall save-func)))))

(defun matrix-client-disconnect (&optional connection)
  "Disconnect from CONNECTION or all Matrix connections, killing room buffers."
  (interactive)
  (let ((connections (if connection
                         (list (cons nil connection))
                       matrix-client-connections)))
    (cl-loop for (_ . con) in connections
             do (progn
                  ;; TODO: Improve the structure of these lists.  It
                  ;; feels inconsistent and confusing.
                  (cl-loop for (_ . room) in (oref con :rooms)
                           do (kill-buffer (oref room :buffer)))
                  (oset con :running nil)))
    (setq matrix-client-connections (seq-difference matrix-client-connections
                                                    connections
                                                    (-lambda ((_ . a-con) (_ . b-con))
                                                      (equal (oref a-con :token) (oref b-con :token)))))))

(cl-defmethod matrix-client-start-watchdog ((con matrix-client-connection) &optional force timer-secs)
  (when (or force matrix-client-enable-watchdog)
    (let ((last-ts (oref con :last-event-ts))
          (next (oref con :end-token))
          (timer (oref con :watchdog-timer)))
      (if timer
          (if (> (* 1000 (- (float-time) last-ts))
                 matrix-client-event-poll-timeout)
              (progn
                ;; Timed out: resync
                (cancel-timer timer)
                ;; XXX Pull these fucking syncs out and bar them on (oref con :running)
                (when (oref con :running)
                  (message "Reconnecting you to Matrix, one moment please...")
                  (cancel-timer timer)
                  (matrix-sync con next nil matrix-client-event-poll-timeout
                               (apply-partially #'matrix-client-sync-handler con))))
            ;; Not timed out: just cancel timer
            (cancel-timer timer)))
      (let ((timer-secs (or timer-secs (/ (* 2 matrix-client-event-poll-timeout) 1000))))
        (oset con :watchdog-timer (run-with-timer timer-secs timer-secs
                                                  (apply-partially #'matrix-client-start-watchdog con)))))))

(cl-defmethod matrix-client-setup-room ((con matrix-client-connection) room-id)
  "Prepare and switch to buffer for ROOM-ID, and return room object."
  (when (get-buffer room-id)
    (kill-buffer room-id))
  (let* ((room-buf (get-buffer-create room-id))
         (room-obj (matrix-client-room room-id :buffer room-buf :con con)))
    (with-current-buffer room-buf
      (matrix-client-mode)
      (setq buffer-undo-list t)
      (erase-buffer)
      (matrix-client-render-message-line room-obj))
    (switch-to-buffer room-buf)
    (set (make-local-variable 'matrix-client-room-connection) con)
    (set (make-local-variable 'matrix-client-room-object) room-obj)
    (push (cons room-id room-obj) (oref con :rooms))
    (oset-multi room-obj
      :id room-id
      :buffer room-buf)
    room-obj))

(cl-defmethod matrix-client-sync-handler ((con matrix-client-connection) data)
  (when (oref con :running)

    ;; Kill buffers for left rooms
    (cl-loop for room in (a-get* data 'rooms 'leave)
             do (let* ((room-id (symbol-name (car room)))
                       (room (matrix-client-room-for-id con room-id)))
                  (when (and room (oref room :buffer))
                    (kill-buffer (oref room :buffer)))))

    ;; Join joined rooms
    (cl-loop for room-data in (a-get* data 'rooms 'join)
             do (let* ((room-id (symbol-name (car room-data)))
                       (room (or (a-get (oref con :rooms) room-id)
                                 (matrix-client-setup-room con room-id)))
                       (room-events (cdr room-data)))
                  ;; For some reason, the events are in arrays instead of lists.
                  (cl-loop for event across (a-get* room-events 'state 'events)
                           do (matrix-client-room-event room event))
                  (cl-loop for event across (a-get* room-events 'timeline 'events)
                           do (matrix-client-room-event room event))))

    ;; FIXME: `matrix-client-invite-room' is unimplemented.  Looking
    ;; at the API <https://matrix.org/docs/spec/client_server/r0.2.0.html#post-matrix-client-r0-rooms-roomid-invite>,
    ;; I'm not sure this is even necessary.
    ;; Process invitations
    ;; (--each (a-get* data 'rooms 'invite)
    ;;   (matrix-client-invite-room con data))

    ;; Process next batch
    (let ((next (map-elt data 'next_batch)))
      (oset-multi con
        :end-token next
        :last-event-ts (float-time))
      (matrix-client-start-watchdog con)
      (matrix-sync con next nil matrix-client-event-poll-timeout
                   (apply-partially #'matrix-client-sync-handler con)))))

(cl-defmethod matrix-client-room-event ((room matrix-client-room) event)
  "Handle state events from a sync."
  (when-let ((con (oref room :con))
             (fns (oref con :event-hook)))
    (--each fns
      (funcall it con room event))))

(cl-defmethod matrix-client-render-event-to-room ((con matrix-client-connection) room item)
  "Feed ITEM in to its proper `matrix-client-event-handlers' handler."
  ;; NOTE: It's tempting to use `map-elt' here, but it uses `eql' to
  ;; compare keys, and since the keys are strings, that doesn't work.
  ;; MAYBE: Perhaps we should change the keys to symbols someday...
  (when-let ((type (a-get item 'type))
             (handler (a-get (oref con :event-handlers) type)))
    (funcall handler con room item)))

(cl-defmethod matrix-client-inject-event-listeners ((con matrix-client-connection))
  "Inject the standard event listeners."
  (unless (oref con :event-hook)
    (oset con :event-hook '(matrix-client-debug-event-maybe
                            matrix-client-render-event-to-room))))

(cl-defmethod matrix-client-debug-event-maybe ((con matrix-client-connection) room data)
  "Debug DATA to *matrix-events* if `matrix-client-debug-events' is non-nil."
  (when matrix-client-debug-events
    (with-current-buffer (get-buffer-create "*matrix-events*")
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n" (prin1-to-string data))))))

(defun matrix-client-update-header-line (room)
  "Update the header line of the current buffer for ROOM."
  ;; Disable when tabbar mode is on
  (unless (and (boundp 'tabbar-mode) tabbar-mode)
    (pcase-let (((eieio typers name topic) room))
      (setq header-line-format (if (> 0 (length typers))
                                   (format "(%d typing...) %s: %s" (length typers) name topic)
                                 (format "%s: %s" name topic))))))

;;;###autoload
(defmacro insert-read-only (text &rest extra-props)
  ;; TODO: Remove this function, and/or move it to matrix-helpers.el.
  "Insert a block of TEXT as read-only, with the ability to add EXTRA-PROPS such as face."
  `(add-text-properties
    (point) (progn
              (insert ,text)
              (point))
    '(read-only t ,@extra-props)))

(cl-defmethod matrix-client-render-message-line ((room matrix-client-room room))
  ;; FIXME: Why is there an extra "room" the arg list?  EIEIO docs
  ;; don't seem to mention this.
  "Insert a message input at the end of the buffer."
  (goto-char (point-max))
  (let ((inhibit-read-only t))
    (insert "\n")
    (insert-read-only "[::] ▶ " rear-nonsticky t)))

(defun matrix-client-send-active-line ()
  "Send the current message-line text after running it through input-filters."
  (interactive)
  (goto-char (point-max))
  (beginning-of-line)
  ;; TODO: Make the prompt character customizable, and probably use
  ;; text-properties or an overlay to find it.
  (re-search-forward "▶")
  (forward-char)
  ;; MAYBE: Just delete the text and store it in a var instead of
  ;; killing it to the kill-ring.  On the one hand, it's a nice
  ;; backup, but some users might prefer not to clutter the kill-ring
  ;; with every message they send.
  (kill-line)
  (let* ((room matrix-client-room-object)
         (con (oref room :con))
         (input-filters (oref con :input-filters)))
    (cl-reduce 'matrix-client-run-through-input-filter
               input-filters
               :initial-value (pop kill-ring))))

(defun matrix-client-run-through-input-filter (text filter)
  "Run each TEXT through a single FILTER.  Used by `matrix-client-send-active-line'."
  (when text
    (funcall filter
             (oref matrix-client-room-object :con)
             text)))

(defun matrix-client-send-to-current-room (con message)
  "Send a string TEXT to the current buffer's room."
  (let* (;; FIXME: Setting `room' here seems to be unnecessary,
         ;; because the function is called in the context of `room'.
         ;; Until adding this comment, the `let*' was `let', so `room'
         ;; was coming from the surrounding context, not this.
         (room matrix-client-room-object)
         ;; FIXME: We shouldn't need to get `con' again here, because
         ;; it's passed to the function.
         (con (when room
                (oref room :con)))
         (room-id (when room
                    (oref room :id))))
    (matrix-send-message con room-id message)))

(defun matrix-client-window-change-hook ()
  "Send a read receipt if necessary."
  ;; FIXME: Unimplemented.
  ;; (when (and matrix-client-room-id matrix-client-room-end-token)
  ;;   (message "%s as read from %s" matrix-client-room-end-token matrix-client-room-id)
  ;;   (matrix-mark-as-read matrix-client-room-id matrix-client-room-end-token))
  )

(provide 'matrix-client)

;;; matrix-client.el ends here
