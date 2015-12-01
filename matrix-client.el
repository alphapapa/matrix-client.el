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

(defvar matrix-client-new-event-hook nil
  "A lists of functions that are evaluated when a new event comes in.")

(defvar matrix-client-event-listener-running nil)

(defvar matrix-client-active-rooms nil
  "Rooms the active client is in.")

(defvar matrix-client-event-handlers '()
  "An alist of (type . function) handler definitions for various matrix types.

Each of these receives the raw event as a single DATA argument.
See `defmatrix-client-handler'.")

(defvar-local matrix-client-room-name nil
  "The name of the buffer's room.")

(defvar-local matrix-client-room-aliases nil
  "The alises of the buffer's room.")

(defvar-local matrix-client-room-topic nil
  "The topic of the buffer's room.")

(defvar-local matrix-client-room-id nil
  "The Matrix ID of the buffer's room.")

(defvar-local matrix-client-room-membership nil
  "The list of members of the buffer's room.")

(defvar-local matrix-client-room-typers nil
  "The list of members of the buffer's room who are currently typing.")

(defvar-local matrix-client-room-end-token nil
  "The most recent event-id in a room, used to push read-receipts to the server.")

(defvar matrix-username nil
  "Your Matrix username.")

(defvar matrix-client-event-stream-end-token nil)

(defvar matrix-client-input-filters nil
  "List of functions to run input through.

Each of these functions take a single argument, the TEXT the user
inputs.  They can modify that text and return a new version of
it, or they can return nil to prevent further processing of it.")

;;;###autoload
(defun matrix-client ()
  "Connect to Matrix.

This will attempt to log you in if you don't have a valid
[`matrix-token'] and will create room buffers for each room you are
in."
  (interactive)
  (unless matrix-token
    (matrix-client-login))
  (matrix-client-inject-event-listeners)
  (matrix-client-handlers-init)
  (let* ((initial-data (matrix-initial-sync 25)))
    (mapc 'matrix-client-set-up-room (matrix-get 'rooms initial-data))
    (message "💣 You're jacked in, welcome to Matrix. (💗♥💓💕)")
    (setq matrix-client-event-listener-running t)
    (matrix-client-start-event-listener (matrix-get 'end initial-data))))

;;;###autoload
(defun matrix-client-login ()
  "Get a token form the Matrix homeserver.

If [`matrix-client-use-auth-source'] is non-nil, attempt to log in
using data from auth-source.  Otherwise, the user will be prompted
for a username and password."
  (interactive)
  (let* ((auth-source-creation-prompts
          '((username . "Matrix identity: ")
            (secret . "Matrix password for %u (homeserver: %h): ")))
         (found (nth 0 (auth-source-search :max 1
                                           :host matrix-homeserver-base-url
                                           :require '(:user :secret)
                                           :create t))))
    (when (and
           found
           (matrix-login-with-password (plist-get found :user)
                                       (let ((secret (plist-get found :secret)))
                                         (if (functionp secret)
                                             (funcall secret)
                                           secret)))
           (setq matrix-username (plist-get found :user))
           (let ((save-func (plist-get found :save-function)))
             (when save-func (funcall save-func)))))))

(defun matrix-client-disconnect ()
  "Disconnect from Matrix and kill all active room buffers."
  (interactive)
  (dolist (room-cons matrix-client-active-rooms)
    (kill-buffer (cdr room-cons)))
  (setq matrix-client-active-rooms nil)
  (setq matrix-client-event-listener-running nil))

(defun matrix-client-reconnect (arg)
  "Reconnect to Matrix.

Without a `prefix-arg' ARG it will simply restart the
matrix-client-stream poller, but with a prefix it will disconnect and
connect, clearing all room data."
  (interactive "P")
  (if (or (not arg) matrix-client-event-stream-end-token)
      (progn
        (matrix-client-stream-from-end-token))
    (progn
      (matrix-client-disconnect)
      (matrix-client))))

(defun matrix-client-set-up-room (roomdata)
  "Set up a room from its initialSync ROOMDATA."
  (let* ((room-id (matrix-get 'room_id roomdata))
         (room-state (matrix-get 'state roomdata))
         (room-messages (matrix-get 'chunk (matrix-get 'messages roomdata)))
         (room-buf (get-buffer-create room-id))
         (room-cons (cons room-id room-buf))
         (render-membership matrix-client-render-membership)
         (render-presence matrix-client-render-presence))
    (setq matrix-client-render-membership nil)
    (setq matrix-client-render-presence nil)
    (add-to-list 'matrix-client-active-rooms room-cons)
    (with-current-buffer room-buf
      (matrix-client-mode)
      (erase-buffer)
      (set (make-local-variable 'matrix-client-room-id) room-id)
      (matrix-client-render-message-line)
      (mapc 'matrix-client-render-event-to-room room-state)
      (mapc 'matrix-client-render-event-to-room room-messages))
    (setq matrix-client-render-membership render-membership)
    (setq matrix-client-render-presence render-presence)
    (switch-to-buffer room-buf)))

(defun matrix-client-window-change-hook ()
  "Send a read receipt if necessary."
  (when (and matrix-client-room-id matrix-client-room-end-token)
    (message "%s as read from %s" matrix-client-room-end-token matrix-client-room-id)
    (matrix-mark-as-read matrix-client-room-id matrix-client-room-end-token)))

(defun matrix-client-start-event-listener (end-tok)
  "Start the event listener if it is not already running, from the END-TOK end token."
  (when matrix-client-event-listener-running
    (matrix-event-poll
     end-tok
     matrix-client-event-poll-timeout
     'matrix-client-event-listener-callback)
    (setq matrix-client-event-stream-end-token end-tok)))

(defun matrix-client-event-listener-callback (data)
  "The callback which `matrix-event-poll' pushes its data in to.

This calls each function in matrix-client-new-event-hook with the data
object with a single argument, DATA."
  (unless (eq (car data) 'error)
    (dolist (hook matrix-client-new-event-hook)
      (funcall hook data)))
  (matrix-client-start-event-listener (matrix-get 'end data)))

(defun matrix-client-inject-event-listeners ()
  "Inject the standard event listeners."
  (add-to-list 'matrix-client-new-event-hook 'matrix-client-debug-event-maybe)
  (add-to-list 'matrix-client-new-event-hook 'matrix-client-render-events-to-room)
  (add-to-list 'matrix-client-new-event-hook 'matrix-client-set-room-end-token))

(defun matrix-client-debug-event-maybe (data)
  "Debug DATA to *matrix-events* if `matrix-client-debug-events' is non-nil."
  (with-current-buffer (get-buffer-create "*matrix-events*")
    (let ((inhibit-read-only t))
      (when matrix-client-debug-events
        (end-of-buffer)
        (insert "\n")
        (insert (prin1-to-string data))))))

(defun matrix-client-render-events-to-room (data)
  "Given a chunk of data from an /initialSyc, render each element from DATA in to its room."
  (let ((chunk (matrix-get 'chunk data)))
    (mapc 'matrix-client-render-event-to-room chunk)))

(defun matrix-client-render-event-to-room (item)
  "Feed ITEM in to its proper `matrix-client-event-handlers' handler."
  (let* ((type (matrix-get 'type item))
         (handler (matrix-get type matrix-client-event-handlers)))
    (when handler
      (funcall handler item))))

(defun matrix-client-update-header-line ()
  "Update the header line of the current buffer."
  (if (> 0 (length matrix-client-room-typers))
      (progn
        (setq header-line-format (format "(%d typing...) %s: %s" (length matrix-client-room-typers)
                                         matrix-client-room-name matrix-client-room-topic)))
    (setq header-line-format (format "%s: %s" matrix-client-room-name matrix-client-room-topic))))

(defun matrix-client-filter (condp lst)
  "A standard filter, feed it a function CONDP and a LST."
  (delq nil
        (mapcar (lambda (x)
                  (and (funcall condp x) x))
                lst)))

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
    (insert-read-only (format "🔥 [%s] ▶" matrix-client-room-id))
    (insert " ")))

(defun matrix-client-send-active-line ()
  "Send the current message-line text after running it through input-filters."
  (interactive)
  (end-of-buffer)
  (beginning-of-line)
  (re-search-forward "▶")
  (forward-char)
  (kill-line)
  (reduce 'matrix-client-run-through-input-filter
          matrix-client-input-filters
          :initial-value (pop kill-ring)))

(defun matrix-client-run-through-input-filter (text filter)
  "Run each TEXT through a single FILTER.  Used by `matrix-client-send-active-line'."
  (when text
    (funcall filter text)))

(defun matrix-client-send-to-current-room (text)
  "Send a string TEXT to the current buffer's room."
  (matrix-send-message matrix-client-room-id text))

(defun matrix-client-set-room-end-token (data)
  "When an event DATA comes in, file it in to the room so that we can mark a cursor when visiting the buffer."
  (mapc (lambda (data)
          (let* ((room-id (matrix-get 'room_id data))
                 (room-buf (matrix-get room-id matrix-client-active-rooms)))
            (when room-buf
              (with-current-buffer room-buf
                (setq matrix-client-room-end-token (matrix-get 'event_id data)))))
          ) (matrix-get 'chunk data)))

(defun matrix-client-restart-listener-maybe (sym error-thrown)
  "The error handler for matrix-client's event-poll.

SYM and ERROR-THROWN come from Request and are used to decide whether to connect."
  (cond ((or (string-match "code 6" (cdr error-thrown))
             (eq sym 'parse-error)
             (eq sym 'timeout)
             (string-match "interrupt" (cdr error-thrown))
             (string-match "code 7" (cdr error-thrown)))
         (message "Lost connection with matrix, will re-attempt in %s ms"
                  (/ matrix-client-event-poll-timeout 2))
         (matrix-client-restart-later))
        ((string-match "code 60" (cdr error-thrown))
         (message "curl couldn't validate CA, not advising --insecure? File bug pls."))))

(defun matrix-client-stream-from-end-token ()
  "Restart the matrix-client stream from the saved end-token."
  (matrix-client-start-event-listener matrix-client-event-stream-end-token))

(defun matrix-client-restart-later ()
  "Try to restart the Matrix poller later, maybe."
  (run-with-timer (/ matrix-client-event-poll-timeout 1000) nil
                  'matrix-client-stream-from-end-token))

(provide 'matrix-client)
;;; matrix-client.el ends here
