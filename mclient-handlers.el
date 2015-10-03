;;; mclient-handlers.el --- Event handlers for Matrix.org RPC

;; Copyright (C) 2015 Ryan Rix
;; Author: Ryan Rix <ryan@whatthefuck.computer>
;; Maintainer: Ryan Rix <ryan@whatthefuck.computer>
;; Created: 21 June 2015
;; Keywords: web
;; Homepage: http://doc.rix.si/matrix.html
;; Package-Version: 0.0.1
;; Package-Requires: ("json")

;; This file is not part of GNU Emacs.

;; mclient-handlers.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.
;;
;; mclient-handlers.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this file.  If not, see <http://www.gnu.org/licenses/>.

(provide 'mclient-handlers)

(defvar mclient-event-handlers '()
  "An alist of (type . function) handler definitions for various matrix types")

(defun mclient-handlers-init ()
  "Set up all the mclient event type handlers"
  (add-to-list 'mclient-event-handlers (cons "m.room.message" 'mclient-handler-m.room.message)))

(defun mclient-handler-m.room.message (data)
  (let* ((room-id (matrix-get 'room_id data))
         (content (matrix-get 'content data))
         (msg-type (matrix-get 'msgtype content))
         (room-buf (matrix-get room-id mclient-active-rooms)))
    (with-current-buffer room-buf
      (end-of-buffer)
      (cond ((string-equal msg-type "m.text")
             (insert "\n")
             (insert (format "<%s> " (matrix-get 'user_id data)))
             (insert (matrix-get 'body content)))
            ((string-equal msg-type "m.image")
             (insert "\n")
             (insert (format "<%s> " (matrix-get 'user_id data)))
             (insert (matrix-get 'body (matrix-get 'content data)))
             (insert ": ")
             (insert (mclient-handlers-transform-mxc-uri (matrix-get 'url content))))))))

(defun mclient-handlers-transform-mxc-uri (uri)
  (let ((components (split-string uri "/")))
    (format "%s/_matrix/media/v1/download/%s/%s"
            matrix-homeserver-base-url
            (elt components 2)
            (elt components 3))))
