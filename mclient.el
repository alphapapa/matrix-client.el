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

(defun mclient-login ()
  "Get a token form the Matrix homeserver.

If [`mclient-use-auth-source'] is non-nil, attempt to log in
using data from auth-source. Otherwise, the user will be prompted
for a username and password.
"
  (interactive)
  (if mclient-use-auth-source
      (let ((pwdata (mclient-read-auth-source)))
        (matrix-login-with-password (cdr (assoc 'username pwdata))
                                    (cdr (assoc 'password pwdata))))
    (let ((username (read-string "Username: "))
          (password (read-string "Password: ")))
      (matrix-login-with-password username password))))

(defun mclient ()
  (interactive)
  (unless matrix-token
    (mclient-login))
  (mclient-inject-event-listeners)
  (let* ((initial-data (matrix-initial-sync 25)))
    (mclient-start-event-listener (cdr (assoc 'end initial-data)))))

(defun mclient-start-event-listener (end-tok)
  (matrix-event-poll
   end-tok
   mclient-event-poll-timeout
   'mclient-event-listener-callback))

(defun mclient-event-listener-callback (status)
  (goto-char url-http-end-of-headers)
  (let ((data (json-read)))
    (dolist (hook mclient-new-event-hook)
      (funcall hook data))
    (mclient-start-event-listener (cdr (assoc 'end data)))))

(defun mclient-inject-event-listeners ()
  "Inject the standard event listeners."
  (add-to-list 'mclient-new-event-hook 'mclient-debug-event-maybe))

(defun mclient-debug-event-maybe (data)
  (with-current-buffer (get-buffer-create "*matrix-events*")
    (end-of-buffer)
    (insert "\n")
    (insert (prin1-to-string data))))
