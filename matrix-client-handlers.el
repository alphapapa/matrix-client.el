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

(cl-defmethod matrix-client-handlers-init ((con matrix-client-connection))
  "Set up all the matrix-client event type handlers.

Each matrix-client-event-handler is an alist of matrix message type and
the function that handles them.  Currently only a single handler
for each event is supported.  The handler takes a single argument,
DATA, which is a `json-read' object from the Event stream.  See
the Matrix spec for more information about its format."
  ;; FIXME: `matrix-client-window-change-hook' should be renamed, and
  ;; is currently unimplemented anyway.
  (push 'matrix-client-window-change-hook window-configuration-change-hook)
  (unless (oref con :event-handlers)
    (oset con :event-handlers (a-list "m.room.message" 'matrix-client-handler-m.room.message
                                      "m.lightrix.pattern" 'matrix-client-handler-m.lightrix.pattern
                                      "m.room.topic" 'matrix-client-handler-m.room.topic
                                      "m.room.name" 'matrix-client-handler-m.room.name
                                      "m.room.member" 'matrix-client-handler-m.room.member
                                      "m.room.aliases" 'matrix-client-handler-m.room.aliases
                                      "m.presence" 'matrix-client-handler-m.presence
                                      "m.typing" 'matrix-client-handler-m.typing)))
  (unless (oref con :input-filters)
    (oset con :input-filters '(matrix-client-input-filter-emote
                               matrix-client-input-filter-join
                               matrix-client-input-filter-leave
                               matrix-client-send-to-current-room))))

(defmacro defmatrix-client-handler (msgtype varlist body)
  "Create an matrix-client-handler.

This macro generates a standard function which provides some
standard variables that each event handler can use to render an
event sanely.  It also sets [`inhibit-read-only'] to true to
allow you to freely render in to the buffer.

MSGTYPE is the type of the message to handle.

Provided Variables:

- `data': data from the event.
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
  ((content (map-elt data 'content))
   (msg-type (map-elt content 'msgtype))
   (format (map-elt content 'format))
   (timestamp (/ (map-elt data 'origin_server_ts) 1000))
   (sender (map-elt data 'sender))
   (display-name (matrix-client-displayname-from-user-id room (map-elt data 'sender)))
   (own-display-name (oref* room :con :username)))

  ((let (metadata output)
     (setq metadata (format "%s %s> "
                            (format-time-string "[%T]" (seconds-to-time timestamp))
                            display-name))
     (when content
       (setq output (pcase msg-type
                      ("m.emote"
                       (concat "* " (map-elt content 'body)))
                      ((guard (and matrix-client-render-html (string= "org.matrix.custom.html" format)))
                       (with-temp-buffer
                         (insert (map-elt content 'formatted_body))
                         (goto-char (point-min))
                         (while (re-search-forward "\\(<br />\\)+" nil t)
                           (replace-match "<br />"))
                         (let ((document (libxml-parse-html-region (point) (point-max))))
                           (erase-buffer)
                           (shr-insert-document document)
                           (goto-char (point-min))
                           (delete-blank-lines)
                           (buffer-string))))
                      ("m.image"
                       (concat (map-elt content 'body)
                               ": "
                               (matrix-transform-mxc-uri (or (map-elt content 'url)
                                                             (map-elt content 'thumbnail_url)))))
                      (t
                       (map-elt content 'body)))))

     ;; Apply face for own messages
     (let (metadata-face message-face)
       (if (string= display-name own-display-name)
           (setq metadata-face 'matrix-client-own-metadata
                 message-face 'matrix-client-own-messages)
         (setq metadata-face 'matrix-client-metadata
               message-face 'default))
       (add-face-text-property 0 (length metadata) metadata-face nil metadata)
       (add-face-text-property 0 (length output) message-face nil output))
     ;; Add metadata to output
     (setq output (concat metadata output))
     ;; Add text properties
     (setq output (propertize output
                              'timestamp timestamp
                              'display-name display-name
                              'sender sender))
     ;; Actually insert text
     (insert-read-only "\n")
     (insert-read-only output))))

(defmatrix-client-handler "m.lightrix.pattern"
  ;; FIXME: Move this to separate file for Ryan.  :)
  ((content (map-elt data 'content)))
  ((insert "\n")
   (insert-read-only (format "🌄 %s --> " (matrix-client-displayname-from-user-id room (map-elt data 'user_id)))
                     face matrix-client-metadata)
   (insert-read-only (map-elt content 'pattern))))

(defmatrix-client-handler "m.room.member"
  ((content (map-elt data 'content))
   (user-id (map-elt data 'sender))
   (membership (map-elt content 'membership))
   (room-membership (oref room :membership))
   (display-name (map-elt content 'displayname)))
  ((assq-delete-all user-id room-membership)
   (if (string= "join" membership)
       (progn
         (when matrix-client-render-membership
           (insert-read-only "\n")
           (insert-read-only (format "Joined: %s (%s) --> %s" display-name user-id membership) face matrix-client-metadata))
         (push (cons user-id content) room-membership))
     (when matrix-client-render-membership
       (insert-read-only "\n")
       (insert-read-only (format "Left: %s (%s) --> %s" display-name user-id membership) face matrix-client-metadata)))
   (oset room :membership room-membership)
   (matrix-update-room-name room)))

(defun matrix-update-room-name (room)
  "Update ROOM's buffer's name.
If it only has two members, use the name of the other member.
Otherwise, use the room name or alias."
  (when-let ((username (oref* room :con :username))
             ;; TODO: Make this a preference.  Some users might want
             ;; 1-1 chats always named after the other user, while
             ;; others might want them named with the room name.
             (buffer-name (cond ((when (oref room :membership)
                                   (eq 2 (length (oref room :membership))))
                                 ;; 1-1 chat
                                 (when-let ((username (cl-loop for member in (oref room :membership)
                                                               when (not (equal username (map-elt member 'displayname)))
                                                               return (or (map-elt member 'displayname)
                                                                          (car member)))))
                                   (if (eq (current-buffer) (get-buffer username))
                                       username
                                     (generate-new-buffer-name username))))
                                ((oref room :room-name))
                                ((car (oref room :aliases))))))
    (rename-buffer buffer-name)))

(defun matrix-client-handler-m.presence (data)
  "Insert presence message into events buffer for DATA."
  (when matrix-client-render-presence
    (let* ((inhibit-read-only t)
           (content (map-elt data 'content))
           (user-id (map-elt content 'user_id))
           (presence (map-elt content 'presence))
           (display-name (map-elt content 'displayname)))
      (with-current-buffer (get-buffer-create "*matrix-events*")
        (goto-char (point-max))
        (insert-read-only "\n")
        (insert-read-only (format "%s (%s) --> %s" display-name user-id presence) face matrix-client-metadata)))))

(defmatrix-client-handler "m.room.name"
  ()
  ((oset room :room-name (a-get* data 'content 'name))
   (matrix-update-room-name room)
   (insert-read-only "\n")
   (insert-read-only (format "Room name changed --> %s" (oref room :room-name)) face matrix-client-metadata)
   (matrix-client-update-header-line room)))

(defmatrix-client-handler "m.room.aliases"
  ((new-alias-list (a-get* data 'content 'aliases)))
  ((oset room :aliases new-alias-list)
   (matrix-update-room-name room)
   (insert-read-only "\n")
   (insert-read-only (format "Room alias changed --> %s" new-alias-list) face matrix-client-metadata)
   (matrix-client-update-header-line room)))

(defmatrix-client-handler "m.room.topic"
  ((topic (a-get* data 'content 'topic)))
  ((oset room :topic topic)
   (insert-read-only "\n")
   (insert-read-only (format "Room topic changed --> %s" topic) face matrix-client-metadata)
   (matrix-client-update-header-line room)))

(defun matrix-client-handler-m.typing (con room data)
  (with-current-buffer (oref room :buffer)
    (set (make-local-variable 'matrix-client-room-typers) (a-get* data 'content 'user_ids))
    (matrix-client-update-header-line room)))

(defun matrix-client-displayname-from-user-id (room user-id)
  "Return display name for USER-ID in ROOM."
  (let* ((membership (oref room :membership))
         (user (a-get membership user-id))
         (displayname (a-get user 'displayname)))
    (or displayname
        user-id)))

(defun matrix-client-input-filter-join (connection text)
  "Filter /join from input TEXT on CONNECTION.
Return nil if room is joined, otherwise TEXT."
  (if (string-match "^/j\\(oin\\)? +\\(.*\\)" text)
      (progn
        (let ((room (substring text (match-beginning 2) (match-end 2))))
          (matrix-join-room connection room)
          (matrix-client-setup-room connection room))
        nil)
    text))

(defun matrix-client-input-filter-leave (_ text)
  "Filter /leave from input TEXT.
Return nil if room is left, otherwise TEXT."
  (if (string-match (rx bos "/leave" (or space eos)) text)
      (pcase-let (((eieio id con) matrix-client-room-object))
        (when (matrix-leave-room con id)
          (kill-buffer)
          nil))
    text))

(defun matrix-client-input-filter-emote (con text)
  "Input filter to handle emotes.  Filters TEXT."
  (if (string-match "^/me +\\(.*\\)" text)
      (let ((emote (substring text (match-beginning 1) (match-end 1)))
            (room-id (and (slot-boundp matrix-client-room-object :id)
                          (oref matrix-client-room-object :id)))
            (con (and (slot-boundp matrix-client-room-object :con)
                      (oref matrix-client-room-object :con))))
        (when (and con room-id)
          (matrix-send-event con room-id "m.room.message"
                             `(("msgtype" . "m.emote")
                               ("body" . ,emote)))
          nil))
    text))

(provide 'matrix-client-handlers)
;;; matrix-client-handlers.el ends here
