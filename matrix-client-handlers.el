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

(require 'browse-url)
(require 'shr)
(require 'url-handlers)

(require 'f)

(require 'matrix-notifications)

(defcustom matrix-client-show-images nil
  "Download and show images posted to rooms.
Images are downloaded synchronously, so images that download
slowly may cause Emacs to appear to hang."
  :type 'boolean)

(defcustom matrix-client-image-url-prefixes
  (list (rx bow "http" (optional "s") "://"
            (or "i.imgur.com" "i.redd.it")
            "/"))
  "List of regexps matching parts of URLs to images that should be downloaded and displayed.
Each regexp should match from the beginning of the URL, including
the protocol type, if desired.  It will automatically be extended
to match until the next whitespace character."
  :type '(repeat string))

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
  (with-slots (event-handlers input-filters) con
    (unless event-handlers
      (setq event-handlers (a-list "m.room.message" 'matrix-client-handler-m.room.message
                                   "m.lightrix.pattern" 'matrix-client-handler-m.lightrix.pattern
                                   "m.room.topic" 'matrix-client-handler-m.room.topic
                                   "m.room.name" 'matrix-client-handler-m.room.name
                                   "m.room.member" 'matrix-client-handler-m.room.member
                                   "m.room.aliases" 'matrix-client-handler-m.room.aliases
                                   "m.presence" 'matrix-client-handler-m.presence
                                   "m.typing" 'matrix-client-handler-m.typing)))
    (unless input-filters
      (setq input-filters '(matrix-client-input-filter-emote
                            matrix-client-input-filter-join
                            matrix-client-input-filter-leave
                            matrix-client-send-to-current-room)))))

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

(defun matrix-client-linkify-urls (text)
  "Return TEXT with URLs in it made clickable and images inserted."
  (with-temp-buffer
    (insert text)
    ;; Insert images
    (when matrix-client-show-images
      (cl-loop for regexp in matrix-client-image-url-prefixes
               for regexp = (rx-to-string `(seq (regexp ,regexp) (1+ (not space))))
               do (goto-char (point-min))
               while (re-search-forward regexp nil 'noerror)
               for url = (match-string 0)
               for image = (with-current-buffer (oref room :buffer)
                             ;; Use room buffer so image can be resized to it
                             (matrix-client--url-to-image url))
               when image
               do (insert image)))
    ;; Linkify URLs
    (goto-char (point-min))
    (cl-loop while (re-search-forward (rx bow "http" (optional "s") "://" (1+ (not space))) nil 'noerror)
             do (make-text-button (match-beginning 0) (match-end 0)
                                  'mouse-face 'highlight
                                  'face 'link
                                  'help-echo (match-string 0)
                                  'action #'browse-url-at-mouse
                                  'follow-link t))
    (buffer-string)))

(defun matrix-client--url-to-image (url)
  "Download image from URL and return a string to insert in the room buffer containing the image for display."
  (if-let ((image-file (url-file-local-copy url))
           (image-data (prog1
                           (f-read-bytes image-file)
                         (f-delete image-file)))
           (image (shr-rescale-image image-data)))
      (with-temp-buffer
        ;; Newline helps prevent wide images from wrapping off the
        ;; edge of the buffer.
        (insert "\n")
        (insert-image image)
        (buffer-string))))

(defmatrix-client-handler "m.room.message"
  ((content (map-elt data 'content))
   (msg-type (map-elt content 'msgtype))
   (format (map-elt content 'format))
   (timestamp (/ (map-elt data 'origin_server_ts) 1000))
   (sender (map-elt data 'sender))
   (display-name (matrix-client-displayname-from-user-id room (map-elt data 'sender)))
   (own-display-name (oref* room :con :username)))

  ((pcase-let (((map event_id) data)
               (metadata)
               (output))
     (setq metadata (format "%s %s> "
                            (format-time-string "[%T]" (seconds-to-time timestamp))
                            display-name))
     (when content
       (setq message (pcase msg-type
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
                                (if-let ((url (matrix-transform-mxc-uri (or (map-elt content 'url)
                                                                            (map-elt content 'thumbnail_url))))
                                         (show-images matrix-client-show-images)
                                         (image (matrix-client--url-to-image url)))
                                    image
                                  ;; If `matrix-client-show-images' is nil, or if the image can't be downloaded,
                                  ;; the linkified URL will be returned.
                                  (matrix-client-linkify-urls url))))
                       (t
                        (matrix-client-linkify-urls (map-elt content 'body))))))

     ;; Trim messages because HTML ones can have extra newlines
     (setq message (string-trim message))

     ;; Apply face for own messages
     (let (metadata-face message-face)
       (if (string= display-name own-display-name)
           (setq metadata-face 'matrix-client-own-metadata
                 message-face 'matrix-client-own-messages)
         (setq metadata-face 'matrix-client-metadata
               message-face 'default))
       ;; Use 'append so that link faces are not overridden.
       (add-face-text-property 0 (length metadata) metadata-face 'append metadata)
       (add-face-text-property 0 (length output) message-face 'append output))
     ;; Add metadata to output
     (setq output (concat metadata message))
     ;; Add text properties
     (setq output (propertize output
                              'timestamp timestamp
                              'display-name display-name
                              'sender sender
                              'event_id event_id))
     ;; Actually insert text
     (insert-read-only "\n")
     (insert-read-only output)

     ;; Notification
     (unless (equal own-display-name display-name)
       (run-hook-with-args 'matrix-client-notify-hook "m.room.message" data
                           :room room)))))

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
   (matrix-client-update-name room)))

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
   (matrix-client-update-name room)
   (insert-read-only "\n")
   (insert-read-only (format "Room name changed --> %s" (oref room :room-name)) face matrix-client-metadata)
   (matrix-client-update-header-line room)))

(defmatrix-client-handler "m.room.aliases"
  ((new-alias-list (a-get* data 'content 'aliases)))
  ((oset room :aliases new-alias-list)
   (matrix-client-update-name room)
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

(defun matrix-client-input-filter-emote (_ text)
  "Filter emotes from TEXT.
Return nil if emote sent, otherwise TEXT."
  (if (string-match "^/me +\\(.*\\)" text)
      (pcase-let ((emote (substring text (match-beginning 1) (match-end 1)))
                  ((eieio id con) matrix-client-room-object))
        (when (and con id)
          (matrix-send-event con id "m.room.message"
                             (a-list "msgtype" "m.emote"
                                     "body" emote))
          nil))
    text))

(provide 'matrix-client-handlers)
;;; matrix-client-handlers.el ends here
