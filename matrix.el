;;; matrix.el --- An ELisp client for the Matrix.org RPC

;; Copyright (C) 2015 Ryan Rix
;; Author: Ryan Rix <ryan@whatthefuck.computer>
;; Maintainer: Ryan Rix <ryan@whatthefuck.computer>
;; Created: 21 June 2015
;; Keywords: web
;; Homepage: http://doc.rix.si/matrix.html
;; Package-Version: 0.0.1
;; Package-Requires: ("json" "oauth2")

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

(defvar matrix-homeserver-base-url "https://matrix.org/_matrix/client/api/v1"
  "URI to your Matrix homeserver, defaults to the official homeserver.")

(defvar matrix-token nil)
(defvar matrix-txn-id nil)

(defun matrix-initial-sync (&optional limit)
  "Perform /initialSync."
  (matrix-send "GET" "/initialSync" (cons 'limit limit)))

(defun matrix-login (login-type arg-list)
  "Attempt to log in to the Matrix homeserver.

LOGIN-TYPE is the value for the `type' key.
ARG-LIST is an alist of additional key/values to add to the submitted JSON."
  (matrix-send "POST" "/login" (add-to-list 'arg-list (cons "type" login-type))))

(defun matrix-send (method path &optional content query-params headers)
  (let* ((url-request-method(upcase method))
         (url-request-extra-headers (add-to-list 'headers '("Content-Type" . "application/json")))
         (query-params (when matrix-token
                         (add-to-list 'query-params (cons "access_token" matrix-token))))
         (query-params (url-build-query-string query-params))
         (url-request-data (json-encode content))
         (endpoint (concat matrix-homeserver-base-url path (unless (eq "" query-params)
                                                             (concat "&" query-params)))))
    (with-current-buffer
        (url-retrieve-synchronously endpoint)
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun matrix-login-with-password (username password)
  (let ((resp 
         (matrix-login "m.login.password" (list (cons "user" username) (cons "password" password)))))
    (setq matrix-token (cdr (assoc 'access_token resp)))
    resp))
