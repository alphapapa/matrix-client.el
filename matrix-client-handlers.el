;;; matrix-client-handlers.el --- Event handlers for Matrix.org RPC

;; Copyright (C) 2015 Ryan Rix
;; Author: Ryan Rix <ryan@whatthefuck.computer>
;; Maintainer: Ryan Rix <ryan@whatthefuck.computer>
;; Created: 21 June 2015
;; Keywords: web
;; Homepage: http://doc.rix.si/matrix.html
;; Package-Version: 0.1.0
;; Package-Requires: ((json))

;; This file is not part of GNU Emacs.

;; matrix-client-handlers.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.
;;
;; matrix-client-handlers.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file holds the standard matrix-client handlers and input filters. See the docstring of
;; [`matrix-client-handlers-init'] and [`defmatrix-client-handler'] for information about these.

;;; Code:

(defmethod matrix-client-handlers-init ((con matrix-client-connection))
  "Set up all the matrix-client event type handlers.

Each matrix-client-event-handler is an alist of matrix message type and
the function that handles them.  Currently only a single handler
for each event is supported.  The handler takes a single argument,
DATA, which is a `json-read' object from the Event stream.  See
the Matrix spec for more information about its format."
  (add-to-list 'window-configuration-change-hook 'matrix-client-window-change-hook)
  (unless (slot-boundp con :event-handlers)
    (oset con :event-handlers
          '(("m.room.message" . matrix-client-handler-m.room.message)
            ("m.lightrix.pattern" . matrix-client-handler-m.lightrix.pattern)
            ("m.room.topic" . matrix-client-handler-m.room.topic)
            ("m.room.name" . matrix-client-handler-m.room.name)
            ("m.room.member" . matrix-client-handler-m.room.member)
            ("m.room.aliases" . matrix-client-handler-m.room.aliases)
            ("m.presence" . matrix-client-handler-m.presence)
            ("m.typing" . matrix-client-handler-m.typing))))
  (unless (slot-boundp con :input-filters)
    (oset con :input-filters
          '(matrix-client-send-to-current-room
            matrix-client-input-filter-emote
            matrix-client-input-filter-join
            matrix-client-input-filter-leave))))

(defmacro defmatrix-client-handler (msgtype varlist body)
  "Create an matrix-client-handler.

This macro generates a standard function which provides some
standard variables that each event handler can use to render an
event sanely.  It also sets [`inhibit-read-only'] to true to
allow you to freely render in to the buffer.

MSGTYPE is the type of the message to handle.

Provided Variables:

- `room': the `matrix-client-room' object that represents the room.
- `room-id': the Matrix room id the message is intended for
- `room-buf': the buffer tied to the Matrix room which the
  message is intended for.
- Any other variables in VARLIST are provided as well.

BODY is the function itself.  See, for example,
[`matrix-client-handler-m.presence'] for an example of what this looks
like."
  (let ((fname (intern (format "matrix-client-handler-%s" msgtype))))
    `(defun ,fname (con room data)
       (let* ((inhibit-read-only t)
              (room-id (oref room :id))
              (room-buf (oref room :buffer))
              ,@varlist)
         (with-current-buffer room-buf
           (save-excursion
             (goto-char (point-max))
             (forward-line -1)
             (end-of-line)
             ,@body))))))

(defmatrix-client-handler "m.room.message"
  ((content (matrix-get 'content data))
   (msg-type (matrix-get 'msgtype content))
   (format (matrix-get 'format content)))
  ((insert-read-only "\n")
   (insert-read-only (format "[::] %s %s> "
                             (format-time-string
                              "[%T]" (seconds-to-time (/ (matrix-get 'origin_server_ts data) 1000)))
                             (matrix-client-displayname-from-user-id
                              room (matrix-get 'sender data))) face matrix-client-metadata)
   (when content
     (cond ((string-equal "m.emote" msg-type)
            (insert-read-only "* ")
            (insert-read-only (matrix-get 'body content)))
           ((and matrix-client-render-html (string-equal "org.matrix.custom.html" format))
            (let* ((bufferstring (with-temp-buffer
                                   (insert (matrix-get 'formatted_body content))
                                   (beginning-of-buffer)
                                   (while (re-search-forward "\\(<br />\\)+" nil t)
                                     (replace-match "<br />"))
                                   (beginning-of-buffer)
                                   (let* ((document (libxml-parse-html-region (point) (point-max))))
                                     (with-temp-buffer
                                       (shr-insert-document document)
                                       (beginning-of-buffer)
                                       (delete-blank-lines)
                                       (buffer-string))))))
              (insert-read-only bufferstring)))
           ((string-equal "m.image" msg-type)
            (insert-read-only (matrix-get 'body content))
            (insert-read-only ": ")
            (insert-read-only (matrix-transform-mxc-uri (matrix-get 'url content))))
           (t
            (insert-read-only (matrix-get 'body content)))))))

(defmatrix-client-handler "m.lightrix.pattern"
  ((content (matrix-get 'content data)))
  ((insert "\n")
   (insert-read-only (format "🌄 %s --> " (matrix-client-displayname-from-user-id room (matrix-get 'user_id data)))
                     face matrix-client-metadata)
   (insert-read-only (matrix-get 'pattern content))))

(defmatrix-client-handler "m.room.member"
  ((content (matrix-get 'content data))
   (user-id (matrix-get 'sender data))
   (membership (matrix-get 'membership content))
   (room-membership (and (slot-boundp room :membership)
                         (oref room :membership)))
   (display-name (matrix-get 'displayname content)))
  ((assq-delete-all user-id room-membership)
   (when (string-equal "join" membership)
     (add-to-list 'room-membership (cons user-id content)))
   (oset room :membership room-membership)
   (matrix-update-room-name room)
   (when matrix-client-render-membership
     (insert-read-only "\n")
     (insert-read-only (format "🚪 %s (%s) --> %s" display-name user-id membership) face matrix-client-metadata))))

(defun matrix-update-room-name (room)
  "If a room has a name, rename the buffer; if a room has only two
  people in it use the membership for the buffer name."
  (let* ((username (and (slot-boundp (oref room :con) :username)
                        (oref (oref room :con) :username)))
         (name (cond ((and (slot-boundp room :room-name)
                           (> (length (oref room :room-name)) 0))
                      (oref room :room-name))
                     ((and (slot-boundp room :aliases)
                           (> (length (oref room :aliases)) 0))
                      (elt (oref room :aliases) 0))
                     ((and (slot-boundp room :membership)
                           (eq (length (oref room :membership)) 2))
                      (let* ((user (elt (matrix-client-filter
                                         (lambda (member)
                                           (not (eq username (first member))))
                                         (oref room :membership))
                                        0))
                             (buf (or (matrix-get 'displayname user)
                                      (elt user 0))))
                        (if buf
                            (if (eq (current-buffer) (get-buffer buf))
                                buf
                              (generate-new-buffer-name buf))))))))
    (when name (rename-buffer name))))

(defun matrix-client-handler-m.presence (data)
  (let* ((inhibit-read-only t)
         (content (matrix-get 'content data))
         (user-id (matrix-get 'user_id content))
         (presence (matrix-get 'presence content))
         (display-name (matrix-get 'displayname content)))
    (with-current-buffer (get-buffer-create "*matrix-events*")
      (when matrix-client-render-presence
        (end-of-buffer)
        (insert-read-only "\n")
        (insert-read-only (format "%s (%s) --> %s" display-name user-id presence) face matrix-client-metadata)))))

(defmatrix-client-handler "m.room.name"
  ()
  ((oset room :room-name (matrix-get 'name (matrix-get 'content data)))
   (matrix-update-room-name room)
   (insert-read-only "\n")
   (insert-read-only (format "Room name changed --> %s" (oref room :room-name)) face matrix-client-metadata)
   (matrix-client-update-header-line room)))

(defmatrix-client-handler "m.room.aliases"
  ((new-alias (matrix-get 'name (matrix-get 'content data)))
   (old-aliases (and (slot-boundp room :aliases)
                     (oref room :aliases)))
   (new-alias-list (append old-aliases
                           (list new-alias))))
  ((oset room :aliases new-alias-list)
   (matrix-update-room-name room)
   (insert-read-only "\n")
   (insert-read-only (format "Room alias changed --> %s" new-alias-list) face matrix-client-metadata)
   (matrix-client-update-header-line room)))

(defmatrix-client-handler "m.room.topic"
  ()
  ((set (make-local-variable 'matrix-client-room-topic) (matrix-get 'topic (matrix-get 'content data)))
   (insert-read-only "\n")
   (insert-read-only (format "Room topic changed --> %s" matrix-client-room-topic) face matrix-client-metadata)
   (matrix-client-update-header-line room)))

(defun matrix-client-handler-m.typing (data)
  (with-current-buffer (matrix-get (matrix-get 'room_id data) matrix-client-active-rooms)
    (set (make-local-variable 'matrix-client-room-typers) (matrix-get 'user_ids (matrix-get 'content data)))
    (matrix-client-update-header-line room)))

(defun matrix-client-displayname-from-user-id (room user-id)
  "Get the Display name for a USER-ID."
  (let* ((membership (and (slot-boundp room :membership)
                          (oref room :membership)))
         (userdata (cdr (assoc user-id membership)))
         (displayname (matrix-get 'displayname userdata)))
    (or displayname
        user-id)))

(defun matrix-client-input-filter-join (text)
  "Input filter to handle JOINs.  Filters TEXT."
  (if (string-match "^/j\\(oin\\)? +\\(.*\\)" text)
      (progn
        (let ((room (substring text (match-beginning 2) (match-end 2))))
          (matrix-client-set-up-room
           (matrix-sync-room
            (matrix-join-room room))))
        nil)
    text))

(defun matrix-client-input-filter-leave (text)
  "Input filter to handle LEAVEs.  Filters TEXT."
  (if (and (string-match "^/leave.*" text)
           (matrix-leave-room matrix-client-room-id))
      (progn
        (kill-buffer)
        nil)
    text))

(defun matrix-client-input-filter-part (text)
  "Input filter to handle PARTs.  Filters TEXT."
  (if (string-match "^/part.*" text)
      (progn
        (matrix-leave-room matrix-client-room-id)
        nil)
    text))

(defun matrix-client-input-filter-emote (text)
  "Input filter to handle emotes.  Filters TEXT."
  (if (string-match "^/me +\\(.*\\)" text)
      (let ((emote (substring text (match-beginning 1) (match-end 1))))
        (matrix-send-event matrix-client-room-id "m.room.message"
                           `(("msgtype" . "m.emote")
                             ("body" . ,emote)))
        nil)
    text))

(provide 'matrix-client-handlers)
;;; matrix-client-handlers.el ends here
