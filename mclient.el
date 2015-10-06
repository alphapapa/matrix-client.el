;;; mclient.el --- A minimal chat client for the Matrix.org RPC

;; Copyright (C) 2015 Ryan Rix
;; Author: Ryan Rix <ryan@whatthefuck.computer>
;; Maintainer: Ryan Rix <ryan@whatthefuck.computer>
;; Created: 21 June 2015
;; Keywords: web
;; Homepage: http://doc.rix.si/matrix.html
;; Package-Version: 0.0.1
;; Package-Requires: ("json")

;; This file is not part of GNU Emacs.

;; mclient.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.
;;
;; mclient.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this file.  If not, see <http://www.gnu.org/licenses/>.

(provide 'mclient)
(require 'mclient-handlers)
(require 'mclient-modes)

(defcustom mclient-use-auth-source nil
  "When non-nil, attempt to load client username and password
  from authinfo.  XXX: Fill in information about format"
  :type 'boolean
  :group 'matrix-client)

(defcustom mclient-debug-events nil
  "When non-nil, log all events to *matrix-events* buffer"
  :type 'boolean
  :group 'matrix-client)

(defcustom mclient-event-poll-timeout 30000
  "How long to wait for a Matrix event in the EventStream before
  timing out and trying again"
  :type 'number
  :group 'matrix-client)

(defvar mclient-new-event-hook nil
  "A lists of functions that are evaluated when a new event comes
  in.")

(defvar mclient-event-listener-running nil)

(defvar mclient-active-rooms nil
  "Rooms the active client is in")

(defvar mclient-event-handlers '()
  "An alist of (type . function) handler definitions for various matrix types")

(defvar-local mclient-room-name nil
  "The name of the room; bufferlocal")

(defvar-local mclient-room-topic nil
  "The name of the room; bufferlocal")

(defvar-local mclient-room-id nil
  "The Matrix ID of the active room; bufferlocal")

(defvar-local mclient-room-membership nil
  "The name of the room; bufferlocal")

(defvar-local mclient-room-typers nil
  "A list of users currently typing in the room. bufferlocal")

(defvar-local mclient-room-end-token nil
  "The most recent event-id, used to push read-receipts to the server")

(defvar mclient-render-presence t
  "Show presence changes in the main buffer windows")

(defvar mclient-render-membership t
  "Show membership changes in the main buffer windows")

(defcustom mclient-backfill-count 10
  "How many messages to backfill at a time when scrolling")

(defcustom mclient-backfill-threshold 5
  "How close to the top of a buffer point needs to be before
  backfilling events.")

(defvar mclient-event-stream-end-token nil)
(defvar mclient-last-poll-buffer nil)

(defun mclient ()
  (interactive)
  (unless matrix-token
    (mclient-login))
  (mclient-inject-event-listeners)
  (mclient-handlers-init)
  (setq mclient-watchdog-timer (run-with-timer mclient-event-poll-timeout
                                               mclient-event-poll-timeout
                                               'mclient-check-idle-timeout))
  (let* ((initial-data (matrix-initial-sync 25)))
    (mapc 'mclient-set-up-room (matrix-get 'rooms initial-data))
    (message "💣 You're jacked in, welcome to Matrix. (💗♥️rrix💓💕)")
    (setq mclient-event-listener-running t)
    (mclient-start-event-listener (matrix-get 'end initial-data))))

(defun mclient-login ()
  "Get a token form the Matrix homeserver.

If [`mclient-use-auth-source'] is non-nil, attempt to log in
using data from auth-source. Otherwise, the user will be prompted
for a username and password.
"
  (interactive)
  (if mclient-use-auth-source
      (let ((pwdata (mclient-read-auth-source)))
        (matrix-login-with-password (matrix-get 'username pwdata)
                                    (matrix-get 'password pwdata)))
    (let ((username (read-string "Username: "))
          (password (read-string "Password: ")))
      (matrix-login-with-password username password))))

(defun mclient-set-up-room (roomdata)
  (let* ((room-id (matrix-get 'room_id roomdata))
         (room-state (matrix-get 'state roomdata))
         (room-messages (matrix-get 'chunk (matrix-get 'messages roomdata)))
         (room-buf (get-buffer-create room-id))
         (room-cons (cons room-id room-buf))
         (render-membership mclient-render-membership)
         (render-presence mclient-render-presence))
    (setq mclient-render-membership nil)
    (setq mclient-render-presence nil)
    (add-to-list 'mclient-active-rooms room-cons)
    (with-current-buffer room-buf
      (matrix-client-mode)
      (erase-buffer)
      (mclient-render-message-line)
      (setq-local mclient-room-id room-id)
      (mapc 'mclient-render-event-to-room room-state)
      (mapc 'mclient-render-event-to-room room-messages))
    (setq mclient-render-membership render-membership)
    (setq mclient-render-presence render-presence)))

(defun mclient-start-event-listener (end-tok)
  (when mclient-event-listener-running
    (setq mclient-last-poll-buffer
          (matrix-event-poll
           end-tok
           mclient-event-poll-timeout
           'mclient-event-listener-callback))
    (setq mclient-event-stream-end-token end-tok)))

(defun mclient-event-listener-callback (status)
  (goto-char url-http-end-of-headers)
  (let ((data (json-read)))
    (dolist (hook mclient-new-event-hook)
      (funcall hook data))
    (mclient-start-event-listener (matrix-get 'end data))))

(defun mclient-check-idle-timeout ()
  (unless mclient-last-poll-buffer
    (message "Matrix timed out, re-connecting to stream")
    (mclient-start-event-listener mclient-event-stream-end-token)))

(defun mclient-inject-event-listeners ()
  "Inject the standard event listeners."
  (add-to-list 'mclient-new-event-hook 'mclient-debug-event-maybe)
  (add-to-list 'mclient-new-event-hook 'mclient-render-events-to-room)
  (add-to-list 'mclient-new-event-hook 'mclient-set-room-end-token))

(defun mclient-debug-event-maybe (data)
  (with-current-buffer (get-buffer-create "*matrix-events*")
    (let ((inhibit-read-only t))
      (when mclient-debug-events
        (end-of-buffer)
        (insert "\n")
        (insert (prin1-to-string data))))))

(defun mclient-render-events-to-room (data)
  (let ((chunk (matrix-get 'chunk data)))
    (mapc 'mclient-render-event-to-room chunk)))

(defun mclient-render-event-to-room (item)
  (let* ((type (matrix-get 'type item))
         (handler (matrix-get type mclient-event-handlers)))
    (when handler
      (funcall handler item))))

(defun mclient-update-header-line ()
  "Update the header line of the current buffer."
  (if (> 0 (length mclient-room-typers))
      (progn
        (setq header-line-format (format "(%d typing...) %s: %s" (length mclient-room-typers)
                                         mclient-room-name mclient-room-topic)))
    (setq header-line-format (format "%s: %s" mclient-room-name mclient-room-topic))))

(defun mclient-disconnect ()
  (interactive)
  (dolist (room-cons mclient-active-rooms)
    (kill-buffer (cdr room-cons)))
  (cancel-timer mclient-watchdog-timer)
  (setq mclient-active-rooms nil)
  (setq mclient-event-listener-running nil))

(defun mclient-reconnect ()
  (interactive)
  (mclient-disconnect)
  (mclient))

(defun mclient-filter (condp lst)
  (delq nil
        (mapcar (lambda (x)
                  (and (funcall condp x) x))
                lst)))

(defmacro insert-read-only (text &rest extra-props)
  `(add-text-properties
    (point) (progn
              (insert ,text)
              (point))
    '(read-only t ,@extra-props)))

(defun mclient-render-message-line ()
  (end-of-buffer)
  (let ((inhibit-read-only t))
    (insert "\n")
    (insert-read-only (format "🔥 [%s] ▶" mclient-room-id))
    (insert " ")))

(defun mclient-send-active-line ()
  (interactive)
  (end-of-buffer)
  (beginning-of-line)
  (re-search-forward "▶")
  (forward-char)
  (kill-line)
  (matrix-send-message mclient-room-id 
                       (pop kill-ring)))

(defun mclient-set-room-end-token (data)
  "When an event comes in, file it in to the room so that we can
  mark a cursor when visiting the buffer."
  (mapc (lambda (data)
          (let* ((room-id (matrix-get 'room_id data))
                 (room-buf (matrix-get room-id mclient-active-rooms)))
            (when room-buf
              (with-current-buffer room-buf
                (setq-local mclient-room-end-token (matrix-get 'event_id data)))))
          ) (matrix-get 'chunk data)))

(defun mclient-window-change-hook ()
  "Send a read receipt if necessary."
  (when (and mclient-room-id mclient-room-end-token)
    (matrix-mark-as-read mclient-room-id mclient-room-end-token)))

