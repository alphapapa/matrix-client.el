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
  (setq mclient-event-handlers nil)
  (setq mclient-input-filters nil)
  (add-to-list 'window-configuration-change-hook 'mclient-window-change-hook)
  (add-to-list 'mclient-event-handlers '("m.room.message" . mclient-handler-m.room.message))
  (add-to-list 'mclient-event-handlers '("m.lightrix.pattern" . mclient-handler-m.lightrix.pattern))
  (add-to-list 'mclient-event-handlers '("m.room.topic" . mclient-handler-m.room.topic))
  (add-to-list 'mclient-event-handlers '("m.room.name" . mclient-handler-m.room.name))
  (add-to-list 'mclient-event-handlers '("m.room.member" . mclient-handler-m.room.member))
  (add-to-list 'mclient-event-handlers '("m.room.aliases" . mclient-handler-m.room.aliases))
  (add-to-list 'mclient-event-handlers '("m.presence" . mclient-handler-m.presence))
  (add-to-list 'mclient-event-handlers '("m.typing" . mclient-handler-m.typing))
  (add-to-list 'mclient-input-filters 'mclient-send-to-current-room)
  (add-to-list 'mclient-input-filters 'mclient-input-filter-join)
  (add-to-list 'mclient-input-filters 'mclient-input-filter-leave))

(defmacro defmclient-handler (msgtype varlist body)
  (let ((fname (intern (format "mclient-handler-%s" msgtype))))
    `(defun ,fname (data)
       (let* ((inhibit-read-only t)
              (room-id (matrix-get 'room_id data))
              (room-buf (matrix-get room-id mclient-active-rooms))
              ,@varlist)
         (with-current-buffer room-buf
           (save-excursion
             (end-of-buffer)
             (previous-line)
             (end-of-line)
             ,@body))))))

(defmclient-handler "m.room.message"
  ((content (matrix-get 'content data))
   (msg-type (matrix-get 'msgtype content)))
  ((insert-read-only "\n")
   (insert-read-only (format "📩 %s %s> "
                             (format-time-string "[%T]" (seconds-to-time (/ (matrix-get 'origin_server_ts data) 1000)))
                             (mclient-displayname-from-user-id (matrix-get 'user_id data))) face mclient-metadata)
   (cond ((string-equal "m.emote" msg-type)
          (insert-read-only "* ")
          (insert-read-only (matrix-get 'body content)))
         ((string-equal "m.image" msg-type)
          (insert-read-only (matrix-get 'body content))(insert-read-only (matrix-get 'body content))
          (insert-read-only ": ")
          (insert-read-only (matrix-transform-mxc-uri (matrix-get 'url content))))
         (t
          (insert-read-only (matrix-get 'body content))))))

(defmclient-handler "m.lightrix.pattern"
  ((content (matrix-get 'content data)))
  ((insert "\n")
   (insert-read-only (format "🌄 %s --> " (mclient-displayname-from-user-id (matrix-get 'user_id data)))
                     face mclient-metadata)     
   (insert-read-only (matrix-get 'pattern content))))

(defmclient-handler "m.room.member"
  ((content (matrix-get 'content data))
   (user-id (matrix-get 'user_id data))
   (membership (matrix-get 'membership content))
   (display-name (matrix-get 'displayname content)))
  ((unless (boundp 'mclient-room-membership)
     (setq-local mclient-room-membership '()))
   (cond ((string-equal "join" membership)
          (add-to-list 'mclient-room-membership (cons user-id content)))
         ((or (string-equal "leave" membership) (string-equal "ban" membership))
          (setq-local mclient-room-membership
                      (mclient-filter (lambda (item)
                                        (string-equal user-id (car item)))
                                      mclient-room-membership))))
   (when mclient-render-membership
     (insert-read-only "\n")
     (insert-read-only (format "🚪 %s (%s) --> %s" display-name user-id membership) face mclient-metadata))))

(defun mclient-handler-m.presence (data)
  (let* ((inhibit-read-only t)
         (content (matrix-get 'content data))
         (user-id (matrix-get 'user_id content))
         (presence (matrix-get 'presence content))
         (display-name (matrix-get 'displayname content)))
    (with-current-buffer (get-buffer-create "*matrix-events*")
      (when mclient-render-presence
        (end-of-buffer)
        (previous-line)
        (end-of-line)
        (insert-read-only "\n")
        (insert-read-only (format "🚚 %s (%s) --> %s" display-name user-id presence) face mclient-metadata)))))

(defmclient-handler "m.room.name"
  ()
  ((setq-local mclient-room-name (matrix-get 'name (matrix-get 'content data)))
   (cond (mclient-room-name
          (rename-buffer mclient-room-name))
         ((> (length mclient-room-aliases) 0)
          (rename-buffer (elt mclient-room-aliases 0))))
   (insert-read-only "\n")
   (insert-read-only (format "📝 Room name changed --> %s" mclient-room-name) face mclient-metadata)
   (mclient-update-header-line)))

(defmclient-handler "m.room.aliases"
  ()
  ((setq-local mclient-room-aliases (matrix-get 'aliases (matrix-get 'content data)))
   (cond (mclient-room-name
          (rename-buffer mclient-room-name))
         ((> (length mclient-room-aliases) 0)
          (rename-buffer (elt mclient-room-aliases 0))))
   (insert-read-only "\n")
   (insert-read-only (format "📝 Room alias changed --> %s" mclient-room-name) face mclient-metadata)
   (mclient-update-header-line)))

(defmclient-handler "m.room.topic"
  ()
  ((setq-local mclient-room-topic (matrix-get 'topic (matrix-get 'content data)))
   (insert-read-only "\n")
   (insert-read-only (format "✏ Room topic changed --> %s" mclient-room-topic) face mclient-metadata)
   (mclient-update-header-line)))

(defun mclient-handler-m.typing (data)
  (with-current-buffer (matrix-get (matrix-get 'room_id data) mclient-active-rooms)
    (setq-local mclient-room-typers (matrix-get 'user_ids (matrix-get 'content data)))
    (mclient-update-header-line)))

(defun mclient-displayname-from-user-id (user-id)
  (let* ((userdata (cdr (assoc user-id mclient-room-membership))))
    (or (matrix-get 'displayname userdata)
        user-id)))

(defun mclient-input-filter-join (text)
  (when (string-match "^/j\\(oin\\)? +\\(.*\\)" text)
    (let ((room (substring text (match-beginning 2) (match-end 2))))
      (mclient-set-up-room
       (matrix-sync-room
        (matrix-join-room room))))
    t))

(defun mclient-input-filter-leave (text)
  (when (and (string-match "^/leave.*" text)
             (matrix-leave-room mclient-room-id))
    (kill-buffer)
    t))

(defun mclient-input-filter-part (text)
  (when (string-match "^/part.*" text)
    (matrix-leave-room mclient-room-id)
    t))
