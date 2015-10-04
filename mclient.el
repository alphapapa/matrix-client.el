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

(defun mclient ()
  (interactive)
  (unless matrix-token
    (mclient-login))
  (mclient-inject-event-listeners)
  (mclient-handlers-init)
  (let* ((initial-data (matrix-initial-sync 25)))
    (mapc 'mclient-set-up-room (matrix-get 'rooms initial-data))
    (setq mclient-event-listener-running t)
    (mclient-start-event-listener (matrix-get 'end initial-data))))

(defun mclient-start-event-listener (end-tok)
  (when mclient-event-listener-running
    (matrix-event-poll
     end-tok
     mclient-event-poll-timeout
     'mclient-event-listener-callback)))

(defun mclient-event-listener-callback (status)
  (goto-char url-http-end-of-headers)
  (let ((data (json-read)))
    (dolist (hook mclient-new-event-hook)
      (funcall hook data))
    (mclient-start-event-listener (matrix-get 'end data))))

(defun mclient-inject-event-listeners ()
  "Inject the standard event listeners."
  (add-to-list 'mclient-new-event-hook 'mclient-debug-event-maybe)
  (add-to-list 'mclient-new-event-hook 'mclient-render-events-to-room))

(defun mclient-debug-event-maybe (data)
  (with-current-buffer (get-buffer-create "*matrix-events*")
    (end-of-buffer)
    (insert "\n")
    (insert (prin1-to-string data))))

(defun mclient-set-up-room (roomdata)
  (let* ((room-id (matrix-get 'room_id roomdata))
         (room-state (matrix-get 'state roomdata))
         (room-buf (get-buffer-create room-id))
         (room-cons (cons room-id room-buf))
         (render-membership mclient-render-membership)
         (render-presence mclient-render-presence))
    (setq mclient-render-membership nil)
    (setq mclient-render-presence nil)
    (add-to-list 'mclient-active-rooms room-cons)
    (with-current-buffer room-buf
      (erase-buffer)
      (mapc 'mclient-render-event-to-room room-state))
    (setq mclient-render-membership render-membership)
    (setq mclient-render-presence render-presence)))

(defun mclient-disconnect ()
  (interactive)
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
  (setq header-line-format (format "%s: %s" mclient-room-name mclient-room-topic)))
