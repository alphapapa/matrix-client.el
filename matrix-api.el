;;; matrix-api.el --- An ELisp client for the Matrix.org RPC

;; Copyright (C) 2015 Ryan Rix
;; Author: Ryan Rix <ryan@whatthefuck.computer>
;; Maintainer: Ryan Rix <ryan@whatthefuck.computer>
;; Created: 21 June 2015
;; Keywords: web
;; Homepage: http://doc.rix.si/matrix.html
;; Package-Version: 0.1.0

;; This file is not part of GNU Emacs.

;; matrix-api.el is free software: you can redistribute it and/or modify it
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
;; specification 0.1. It forms the basis of the included `matrix-client' Matrix chat
;; client, and can be used as a general RPC system using `matrix-send-event' and
;; `matrix-event-poll'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'request)
(require 'matrix-helpers)
(require 'eieio)

(defvar matrix-error-hook nil
  "This is a list of functions to pass Matrix errors to.")

(defcustom matrix-homeserver-base-url "https://matrix.org"
  "URI to your Matrix homeserver, defaults to the official homeserver."
  :type 'string
  :group 'matrix-client)

(defcustom matrix-insecure-connection nil
  "Whether to use insecure HTTPS connection when connecting to the homeserver."
  :type 'boolean
  :group 'matrix-client)

(defclass matrix-connection ()
  ((base-url :initarg :base-url
             :initform  "https://matrix.org"
             :type string
             :documentation "URI to your Matrix homeserver, defaults to the official homeserver.")
   (token :initarg :token
          :type string
          :custom string
          :documentation "Matrix access_token")
   (txn-id :initarg :txn-id
           :initform 1
           :type integer)))

(defmethod matrix-login ((con matrix-connection) login-type arg-list)
  "Attempt to log in to the Matrix homeserver.

LOGIN-TYPE is the value for the `type' key.
ARG-LIST is an alist of additional key/values to add to the submitted JSON."
  (let ((resp (matrix-send con "POST" "/login" (add-to-list 'arg-list (cons "type" login-type)))))
    (oset con :token (cdr (assoc 'access_token resp)))
    resp))

(defmethod matrix-login-with-password ((con matrix-connection) username password)
  "Given a USERNAME and PASSWORD log in to the homeserver and save the token."
  (matrix-login con "m.login.password" (list (cons "user" username) (cons "password" password))))

(cl-defmethod matrix-request-error-handler
    ((con matrix-connection) &rest args &key error-thrown symbol-status
     &allow-other-keys)
  ;; Call err handler functions
  (dolist (handler matrix-error-hook)
    (funcall handler con symbol-status error-thrown))
  ;; Message some warnings if we know what it is
  (let ((exit-code (matrix-parse-curl-exit-code (cdr error-thrown))))
    (cond ((or (eq exit-code 51)
               (eq exit-code 60))
           (message
            "Error sending request to matrix homeserver, SSL certificate is invalid"))
          ((eq nil exit-code)
           (message
            "Unknown error occurred sending request to matrix homeserver: %S"
            error-thrown))
          (t
           (message "Matrix request exited with exit code %d" exit-code)))))

(defmethod matrix-send
  ((con matrix-connection) method path &optional content
   query-params headers api-version)
  "Send an event to the matrix homeserver.

METHOD is the HTTP method the given API endpoint PATH uses.
CONTENT is an optional `json-encode' compatible list which will
be used as the data in a POST or PUT request.  QUERY-PARAMS is an
optional alist of URL parameters.  HEADERS is optional HTTP
headers to add to the request.

The return value is the `json-read' response from homeserver."
  (let* ((token (and (slot-boundp con :token) (oref con :token)))
         (query-params (when token
                         (add-to-list 'query-params
                                      (cons "access_token" token))))
         (url-request-data (when content (json-encode content)))
         (endpoint (concat (matrix-homeserver-api-url api-version) path)))
    (ad-activate 'request--curl-command)
    (let ((response
           (cond ((string-equal "GET" (upcase method))
                  (request endpoint
                           :type (upcase method)
                           :params query-params
                           :sync t
                           :error
                           (apply-partially #'matrix-request-error-handler con)
                           :parser 'json-read))
                 ((or (string-equal "POST" (upcase method))
                      (string-equal "PUT" (upcase method)))
                  (request endpoint
                           :type (upcase method)
                           :params query-params
                           :sync t
                           :error
                           (apply-partially #'matrix-request-error-handler con)
                           :data (json-encode content)
                           :headers
                           (add-to-list 'headers
                                        '("Content-Type" . "application/json"))
                           :parser 'json-read)))))
      (ad-deactivate 'request--curl-command)
      (request-response-data response))))

(defadvice request--curl-command (around matrix-api-request--with-insecure activate)
  "Advise function to add -k to curl call for `matrix-send-event'."
  (if matrix-insecure-connection
      (setq ad-return-value (append ad-do-it '("--insecure")))
    ad-do-it))

(defmethod matrix-send-async ((con matrix-connection) method path &optional content query-params headers cb api-version)
  "Perform an asynchronous Matrix API call.

METHOD is the HTTP method the given API endpoint PATH uses.
CONTENT is an optional `json-encode' compatible list which will
be used as the data in a POST or PUT request.
QUERY-PARAMS is an optional alist of URL parameters.
HEADERS is optional HTTP headers to add to the request.
CB is the callback which will be called by `request' when the
call completes"
  (let* ((token (oref con token))
         (endpoint (concat (matrix-homeserver-api-url api-version) path)))
    (ad-activate 'request--curl-command)
    (request endpoint
             :type (upcase method)
             :params (when token
                       (add-to-list 'query-params (cons "access_token" token)))
             :parser 'json-read
             :data (json-encode content)
             :error (apply-partially #'matrix-request-error-handler con)
             :headers (add-to-list 'headers '("Content-Type" . "application/json"))
             :complete (apply-partially #'matrix-async-cb-router cb con))
    (ad-deactivate 'request--curl-command)))

(defun* matrix-async-cb-router (cb con &key data error-thrown symbol-status &allow-other-keys)
  (if (or error-thrown (eq symbol-status 'timeout))
      (dolist (handler matrix-error-hook)
        (funcall handler con symbol-status error-thrown))
    (when cb
      (funcall cb data))))

(defmethod matrix-send-event ((con matrix-connection) room-id event-type content)
  "Send a raw event to the room ROOM-ID.

EVENT-TYPE is the matrix event type to send (See the Spec)
CONTENT is a `json-encode' compatible list to include in the event."
  (let* ((txn-id (oref con :txn-id))
         (path (format "/rooms/%s/send/%s/%s"
                       (url-encode-url room-id)
                       (url-encode-url event-type)
                       txn-id)))
    (oset con :txn-id (+ 1 (or txn-id 0)))
    (matrix-send con "PUT" path content)))

(defmethod matrix-send-message ((con matrix-connection) room-id message)
  "Send to the room ROOM-ID the string MESSAGE."
  (matrix-send-event con room-id "m.room.message"
                     (list (cons "msgtype" "m.text")
                           (cons "body" message))))

(defmethod matrix-sync ((con matrix-connection) since full-state timeout cb)
  "Start an event poller starting from END-TOKEN.

It will wait at least TIMEOUT seconds before calling the
CALLBACK.  After receiving any events it will call CALLBACK with
those events as its argument."
  (let ((qparms `(("timeout" . ,(int-to-string timeout)))))
    (add-to-list 'qparms `("full_state" . ,(if full-state
                                               "true"
                                             "false")))
    (when since
      (add-to-list 'qparms `("since" . ,since)))
    (matrix-send-async con "GET" "/sync" nil
                       qparms
                       nil
                       cb
                       "r0")))

(defmethod matrix-event-poll ((con matrix-connection) end-token timeout callback)
  "Start an event poller starting from END-TOKEN.

It will wait at least TIMEOUT seconds before calling the
CALLBACK.  After receiving any events it will call CALLBACK with
those events as its argument."
  (matrix-send-async "GET" "/events" nil
                     (list (cons "from" end-token)
                           (cons "timeout" (number-to-string timeout)))
                     nil callback))

(defmethod matrix-mark-as-read ((con matrix-connection) room-id event-id)
  "In room ROOM-ID mark a given event EVENT-ID as read."
  (let ((path (format "/rooms/%s/receipt/m.read/%s" room-id event-id)))
    (matrix-send-async con "POST" path nil nil nil (lambda (status)) "api/v2_alpha")))

(defmethod matrix-join-room ((con matrix-connection) room-id)
  "Join the room ROOM-ID."
  (let* ((txn-id (oref con :txn-id))
         (path (format "/join/%s" (url-hexify-string room-id))))
    (matrix-get 'room_id (matrix-send con "POST" path (list)))))

(defmethod matrix-leave-room ((con matrix-connection) room-id)
  "Leave the room ROOM-ID."
  (let* ((txn-id (oref con :txn-id))
         (path (format "/rooms/%s/leave" (url-encode-url room-id))))
    (matrix-get 'room_id (matrix-send con "POST" path (list)))))

(defmethod matrix-sync-room ((con matrix-connection) room-id)
  "Perform an /initialSync of a single room ROOM-ID."
  (let* ((txn-id (oref con :txn-id))
         (path (format "/rooms/%s/initialSync" (url-hexify-string room-id))))
    (matrix-send con "GET" path (list))))

(provide 'matrix-api)
;;; matrix-api.el ends here
