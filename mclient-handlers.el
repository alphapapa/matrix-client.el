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

(defun mclient-handlers-init ()
  "Set up all the mclient event type handlers"
  (add-to-list 'mclient-event-handlers '("m.room.message" . mclient-handler-m.room.message))
  (add-to-list 'mclient-event-handlers '("m.lightrix.pattern" . mclient-handler-m.lightrix.pattern))
  (add-to-list 'mclient-event-handlers '("m.room.topic" . mclient-handler-m.room.topic))
  (add-to-list 'mclient-event-handlers '("m.room.name" . mclient-handler-m.room.name))
  (add-to-list 'mclient-event-handlers '("m.room.member" . mclient-handler-m.room.member))
  (add-to-list 'mclient-event-handlers '("m.presence" . mclient-handler-m.presence))
  (add-to-list 'mclient-event-handlers '("m.typing" . mclient-handler-m.typing)))

(defun mclient-handler-m.room.message (data)
  (let* ((room-id (matrix-get 'room_id data))
         (content (matrix-get 'content data))
         (msg-type (matrix-get 'msgtype content))
         (room-buf (matrix-get room-id mclient-active-rooms)))
    (with-current-buffer room-buf
      (end-of-buffer)
      (insert "\n")
      (insert (format "📩 %s %s> "
                      (format-time-string "[%T]" (seconds-to-time (/ (matrix-get 'origin_server_ts data) 1000)))
                      (mclient-displayname-from-user-id (matrix-get 'user_id data))))
      (insert (matrix-get 'body content))
      (cond ((string-equal msg-type "m.image")
             (insert ": ")
             (insert (matrix-transform-mxc-uri (matrix-get 'url content))))))))

(defun mclient-handler-m.lightrix.pattern (data)
  (let* ((room-id (matrix-get 'room_id data))
         (content (matrix-get 'content data))
         (msg-type (matrix-get 'msgtype content))
         (room-buf (matrix-get room-id mclient-active-rooms)))
    (with-current-buffer room-buf
      (end-of-buffer)
      (insert "\n")
      (insert (format "🌄 %s --> " (mclient-displayname-from-user-id (matrix-get 'user_id data))))
      (insert (matrix-get 'pattern content)))))

(defun mclient-handler-m.room.member (data)
  (let* ((room-id (matrix-get 'room_id data))
         (content (matrix-get 'content data))
         (user-id (matrix-get 'user_id data))
         (membership (matrix-get 'membership content))
         (display-name (matrix-get 'displayname content))
         (room-buf (matrix-get room-id mclient-active-rooms)))
    (with-current-buffer room-buf
      (unless (boundp 'mclient-room-membership)
        (setq-local mclient-room-membership '()))
      (cond ((string-equal "join" membership)
             (add-to-list 'mclient-room-membership (cons user-id content)))
            ((or (string-equal "leave" membership) (string-equal "ban" membership))
             (setq-local mclient-room-membership
                         (mclient-filter (lambda (item)
                                           (string-equal user-id (car item)))
                                         mclient-room-membership))))
      (when mclient-render-membership
        (end-of-buffer)
        (insert "\n")
        (insert (format "🚪 %s (%s) --> %s" display-name user-id membership))))))

(defun mclient-handler-m.presence (data)
  (let* ((content (matrix-get 'content data))
         (user-id (matrix-get 'user_id content))
         (presence (matrix-get 'presence content))
         (display-name (matrix-get 'displayname content)))
    (with-current-buffer (get-buffer-create "*matrix-events*")
      (when mclient-render-presence
        (end-of-buffer)
        (insert "\n")
        (insert (format "🚚 %s (%s) --> %s" display-name user-id presence))))))

(defun mclient-handler-m.room.name (data)
  (with-current-buffer (matrix-get (matrix-get 'room_id data) mclient-active-rooms)
    (setq-local mclient-room-name (matrix-get 'name (matrix-get 'content data)))
    (when mclient-room-name
      (rename-buffer mclient-room-name))
    (mclient-update-header-line)))

(defun mclient-handler-m.room.topic (data)
  (with-current-buffer (matrix-get (matrix-get 'room_id data) mclient-active-rooms)
    (setq-local mclient-room-topic (matrix-get 'topic (matrix-get 'content data)))
    (mclient-update-header-line)))

(defun mclient-handler-m.typing (data)
  (with-current-buffer (matrix-get (matrix-get 'room_id data) mclient-active-rooms)
    (setq-local mclient-room-typers (matrix-get 'user_ids (matrix-get 'content data)))
    (mclient-update-header-line)))

(defun mclient-displayname-from-user-id (user-id)
  (let* ((userdata (cdr (assoc user-id mclient-room-membership))))
    (matrix-get 'displayname userdata)))
