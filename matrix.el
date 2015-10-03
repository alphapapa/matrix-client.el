;;; matrix.el --- An ELisp client for the Matrix.org RPC

;; Copyright (C) 2015 Ryan Rix
;; Author: Ryan Rix <ryan@whatthefuck.computer>
;; Maintainer: Ryan Rix <ryan@whatthefuck.computer>
;; Created: 21 June 2015
;; Keywords: web
;; Homepage: http://doc.rix.si/matrix.html
;; Package-Version: 0.0.1
;; Package-Requires: ("json")

;; This file is not part of GNU Emacs.

;; matrix.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.
;;
;; matrix.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this file.  If not, see <http://www.gnu.org/licenses/>.

(provide 'matrix)
(require 'json)

(defcustom matrix-homeserver-base-url "https://matrix.org/_matrix/client/api/v1"
  "URI to your Matrix homeserver, defaults to the official homeserver."
  :type 'string
  :group 'matrix)

(defvar matrix-token nil)
(defvar matrix-txn-id nil)

(defun matrix-initial-sync (&optional limit)
  "Perform /initialSync."
  (matrix-send "GET" "/initialSync" nil (list (list 'limit limit))))

(defun matrix-login (login-type arg-list)
  "Attempt to log in to the Matrix homeserver.

LOGIN-TYPE is the value for the `type' key.
ARG-LIST is an alist of additional key/values to add to the submitted JSON."
  (matrix-send "POST" "/login" (add-to-list 'arg-list (cons "type" login-type))))

(defun matrix-send (method path &optional content query-params headers)
  (let* ((url-request-method(upcase method))
         (url-request-extra-headers (add-to-list 'headers '("Content-Type" . "application/json")))
         (query-params (when matrix-token (add-to-list 'query-params (list "access_token" matrix-token))))
         (query-params (url-build-query-string query-params))
         (url-request-data (when content (json-encode content)))
         (endpoint (concat matrix-homeserver-base-url path (unless (eq "" query-params)
                                                             (concat "?" query-params)))))
    (with-current-buffer
        (url-retrieve-synchronously endpoint)
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun matrix-send-async (method path &optional content query-params headers cb)
  (let* ((url-request-method(upcase method))
         (url-request-extra-headers (add-to-list 'headers '("Content-Type" . "application/json")))
         (query-params (when matrix-token (add-to-list 'query-params (list "access_token" matrix-token))))
         (query-params (url-build-query-string query-params))
         (url-request-data (when content (json-encode content)))
         (endpoint (concat matrix-homeserver-base-url path (unless (eq "" query-params)
                                                             (concat "?" query-params)))))
    (url-retrieve endpoint cb)))

(defun matrix-login-with-password (username password)
  (let ((resp 
         (matrix-login "m.login.password" (list (cons "user" username) (cons "password" password)))))
    (setq matrix-token (cdr (assoc 'access_token resp)))
    resp))

(defun matrix-send-event (room-id event-type content)
  (let* ((txn-id matrix-txn-id)
         (path (format "/rooms/%s/send/%s/%s"
                       (url-encode-url room-id)
                       (url-encode-url event-type)
                       (url-encode-url txn-id))))
    (setq matrix-txn-id (+ 1 (or matrix-txn-id 0)))
    (matrix-send "PUT" path content)))

(defun matrix-send-message (room-id message)
  (matrix-send-event room-id "m.room.message"
                     (list (cons "msgtype" "m.text")
                           (cons "body" message))))

(defun matrix-event-poll (end-token timeout callback)
  (matrix-send-async "GET" "/events" nil (list (list "from" end-token) (list "timeout" timeout)) nil callback))

(defun matrix-get (key obj)
  (cdr (assoc key obj)))
