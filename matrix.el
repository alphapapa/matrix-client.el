;;; matrix.el --- An ELisp client for the Matrix.org RPC

;; Copyright (C) 2015 Ryan Rix
;; Author: Ryan Rix <ryan@whatthefuck.computer>
;; Maintainer: Ryan Rix <ryan@whatthefuck.computer>
;; Created: 21 June 2015
;; Keywords: web
;; Homepage: http://doc.rix.si/matrix.html
;; Package-Version: 0.0.1
;; Package-Requires: ((json) (request))

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

;;; Commentary:

;; This is a pure-elisp implementation of the Matrix.org RPC protocol
;; specification 0.1. It forms the basis of the included `mclient' Matrix chat
;; client, and can be used as a general RPC system using `matrix-send-event' and
;; `matrix-event-poll'.

;;; Code:

(require 'json)
(require 'request)

;;;###autoload
(defcustom matrix-homeserver-base-url "https://matrix.org/"
  "URI to your Matrix homeserver, defaults to the official homeserver."
  :type 'string
  :group 'matrix)

(defvar matrix-token nil)
(defvar matrix-txn-id nil)
(defvar matrix-error-hook nil
  "This is a list of functions to pass Matrix errors to.")

(defun matrix-homeserver-api-url ()
  "Message `matrix-homeserver-base-url' in to a fully-qualified API endpoint URL."
  (format "%s/_matrix/client/api/v1" matrix-homeserver-base-url))

(defun matrix-initial-sync (&optional limit)
  "Perform /initialSync.

Fetch up to LIMIT events for each room."
  (matrix-send "GET" "/initialSync" nil (list (cons 'limit (number-to-string limit)))))

(defun matrix-login (login-type arg-list)
  "Attempt to log in to the Matrix homeserver.

LOGIN-TYPE is the value for the `type' key.
ARG-LIST is an alist of additional key/values to add to the submitted JSON."
  (matrix-send "POST" "/login" (add-to-list 'arg-list (cons "type" login-type))))

(defun matrix-send (method path &optional content query-params headers)
  "Send an event to the matrix homeserver.

METHOD is the HTTP method the given API endpoint PATH uses.
CONTENT is an optional `json-encode' compatible list which will
be used as the data in a POST or PUT request.  QUERY-PARAMS is an
optional alist of URL parameters.  HEADERS is optional HTTP
headers to add to the request.

The return value is the `json-read' response from homeserver."
  (let* ((query-params (when matrix-token (add-to-list 'query-params (cons \"access_token\" matrix-token))))
         (url-request-data (when content (json-encode content)))
         (endpoint (concat (matrix-homeserver-api-url) path)))
    (advice-add 'request--curl-command :filter-return #'request--with-insecure)
    (let ((response (cond ((string-equal \"GET\" (upcase method))
                           (request endpoint
                                    :type (upcase method)
                                    :params query-params
                                    :sync t
                                    :parser 'json-read))
                          ((or (string-equal \"POST\" (upcase method))
                               (string-equal \"PUT\" (upcase method)))
                           (request endpoint
                                    :type (upcase method)
                                    :params query-params
                                    :sync t
                                    :data (json-encode content)
                                    :headers (add-to-list 'headers '(\"Content-Type\" . \"application/json\"))
                                    :parser 'json-read)))))
      (advice-remove 'request--curl-command #'request--with-insecure)
      (request-response-data response))))

(defun request--with-insecure (command-list)
  "Advice function to add -k to curl call for `matrix-send-event'.

COMMAND-LIST is internal request-plumbing."
  (append command-list '("--insecure")))

(defun matrix-send-async (method path &optional content query-params headers cb)
  "Perform an asynchronous Matrix API call.

METHOD is the HTTP method the given API endpoint PATH uses.
CONTENT is an optional `json-encode' compatible list which will
be used as the data in a POST or PUT request.
QUERY-PARAMS is an optional alist of URL parameters.
HEADERS is optional HTTP headers to add to the request.
CB is the callback which will be called by `request' when the
call completes"
  (let* ((endpoint (concat (matrix-homeserver-api-url) path)))
    (advice-add 'request--curl-command :filter-return #'request--with-insecure)
    (request endpoint
             :type (upcase method)
             :params (when matrix-token (add-to-list 'query-params (cons "access_token" matrix-token)))
             :parser 'json-read
             :data (json-encode content)
             :headers (add-to-list 'headers '("Content-Type" . "application/json"))
             :complete (apply-partially #'matrix-async-cb-router cb))
    (advice-remove 'request--curl-command #'request--with-insecure)))

(defun* matrix-async-cb-router (cb &key data error-thrown symbol-status &allow-other-keys)
  (if (or error-thrown (eq symbol-status 'timeout))
      (dolist (handler matrix-error-hook)
        (funcall handler symbol-status error-thrown))
    (when cb
      (funcall cb data))))

(defun matrix-login-with-password (username password)
  "Given a USERNAME and PASSWORD log in to the homeserver and save the token."
  (let ((resp 
         (matrix-login "m.login.password" (list (cons "user" username) (cons "password" password)))))
    (setq matrix-token (cdr (assoc 'access_token resp)))
    resp))

(defun matrix-send-event (room-id event-type content)
  "Send a raw event to the room ROOM-ID.

EVENT-TYPE is the matrix event type to send (See the Spec)
CONTENT is a `json-encode' compatible list to include in the event."
  (let* ((txn-id matrix-txn-id)
         (path (format "/rooms/%s/send/%s/%s"
                       (url-encode-url room-id)
                       (url-encode-url event-type)
                       (url-encode-url txn-id))))
    (setq matrix-txn-id (+ 1 (or matrix-txn-id 0)))
    (matrix-send "PUT" path content)))

(defun matrix-send-message (room-id message)
  "Send to the room ROOM-ID the string MESSAGE."
  (matrix-send-event room-id "m.room.message"
                     (list (cons "msgtype" "m.text")
                           (cons "body" message))))

(defun matrix-event-poll (end-token timeout callback)
  "Start an event poller starting from END-TOKEN.

It will wait at least TIMEOUT seconds before calling the
CALLBACK.  After receiving any events it will call CALLBACK with
those events as its argument."
  (matrix-send-async "GET" "/events" nil
                     (list (cons "from" end-token)
                           (cons "timeout" (number-to-string timeout)))
                     nil callback))

(defun matrix-get (key obj)
  "Easy JSON accessor, get KEY's value from OBJ."
  (cdr (assoc key obj)))

(defun matrix-transform-mxc-uri (uri)
  "Turn an MXC content URI in to an HTTP URL."
  (let ((components (split-string uri "/")))
    (format "%s/_matrix/media/v1/download/%s/%s"
            matrix-homeserver-base-url
            (elt components 2)
            (elt components 3))))

(defun matrix-homeserver-api-url ()
  "Create a v1 API base URL from the homeserver base url."
  (format "%s/_matrix/client/api/v1" matrix-homeserver-base-url))

(defun matrix-homeserver-api-url--v2_alpha ()
  "Create a v2_alpha API base URL from the homeserver base url."
  (format "%s/_matrix/client/v2_alpha" matrix-homeserver-base-url))

(defun matrix-mark-as-read (room-id event-id)
  "In room ROOM-ID mark a given event EVENT-ID as read."
  (cl-letf (((symbol-function 'matrix-homeserver-api-url) #'matrix-homeserver-api-url--v2_alpha))
    (let ((path (format "/rooms/%s/receipt/m.read/%s" room-id event-id)))
      (matrix-send-async "POST" path nil nil nil (lambda (status))))))

(defun matrix-join-room (room-id)
  "Join the room ROOM-ID."
  (let* ((txn-id matrix-txn-id)
         (path (format "/join/%s" (url-hexify-string room-id))))
    (matrix-get 'room_id (matrix-send "POST" path (list)))))

(defun matrix-leave-room (room-id)
  "Leave the room ROOM-ID."
  (let* ((txn-id matrix-txn-id)
         (path (format "/rooms/%s/leave" (url-encode-url room-id))))
    (matrix-get 'room_id (matrix-send "POST" path (list)))))

(defun matrix-sync-room (room-id)
  "Perform an /initialSync of a single room ROOM-ID."
  (let* ((txn-id matrix-txn-id)
         (path (format "/rooms/%s/initialSync" (url-hexify-string room-id))))
    (matrix-send "GET" path (list))))

(provide 'matrix)
;;; matrix.el ends here
