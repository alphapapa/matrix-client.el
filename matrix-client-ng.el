;;; matrix-client-ng.el --- summary ---  -*- lexical-binding: t; -*-

;;; Commentary:

;; Commentary.

;;; Code:

;;;; Requirements

(require 'matrix-api-r0.3.0)
(require 'matrix-notifications)

;;;; TEMP

(cl-defun matrix-client-notify-m.room.message (event &key room &allow-other-keys)
  "Show notification for m.room.message events.
DATA should be the `data' variable from the
`defmatrix-client-handler'.  ROOM should be the room object."
  (pcase-let* (((map content sender event_id) event)
               ((map body) content)
               ((eieio extra) room)
               ((eieio buffer) extra)
               (display-name (matrix-user-displayname room sender))
               (id (notifications-notify :title (format "<b>%s</b>" display-name)
                                         ;; Encode the message as ASCII because dbus-notify
                                         ;; can't handle some Unicode chars.  And
                                         ;; `ignore-errors' doesn't work to ignore errors
                                         ;; from it.  Don't ask me why.
                                         :body (encode-coding-string body 'us-ascii)
                                         :category "im.received"
                                         :timeout 5000
                                         :app-icon nil
                                         :actions '("default" "Show")
                                         :on-action #'matrix-client-notification-show)))
    (map-put matrix-client-notifications id (a-list 'buffer buffer
                                                    'event_id event_id))
    ;; Trim the list
    (setq matrix-client-notifications (-take 20 matrix-client-notifications))))

;;;; Variables

(defvar matrix-client-ng-sessions nil
  "List of active sessions.")

;;;; Classes

(matrix-defclass matrix-client-room-extra ()
  ((buffer :initarg :buffer))
  "Extra data stored in room objects.")

;;;; Mode

(add-hook 'matrix-room-update-hook #'matrix-client-ng-update)

(defun matrix-client-ng ()
  "Matrix Client NG"
  (interactive)
  (if matrix-client-ng-sessions
      ;; TODO: Already have active session: display list of buffers
      ;; FIXME: If login fails, it still shows as active.
      (message "Already active")
    ;; Start new session
    ;; MAYBE: Use auth functions for credentials
    (when-let ((user (read-string "User ID: "))
               (password (read-passwd "Password: "))
               (session (matrix-session :user user))
               (matrix-synchronous t))
      ;; FIXME: Use a callback instead of sync
      (when (matrix-login session password)
        (push session matrix-client-ng-sessions)
        (matrix-sync session)
        (message "Jacked in to %s.  Syncing..." (oref session server))))))

(defun matrix-client-ng-disconnect ()
  "Unplug from the Matrix."
  (interactive)
  ;; MAYBE: Delete buffers.
  (seq-do #'matrix-logout matrix-client-ng-sessions))

;;;; Rooms

;;;;; Metadata

(cl-defmethod matrix-client-ng-metadata-updated ((room matrix-room))
  "Update metadata for ROOM."
  (matrix-client-ng-update-header room))

(add-hook 'matrix-room-metadata-hook #'matrix-client-ng-metadata-updated)

(cl-defmethod matrix-client-ng-update-header ((room matrix-room))
  "Update the header line of the current buffer for ROOM.
Also update prompt with typers."
  (unless (and (boundp 'tabbar-mode) tabbar-mode)
    ;; Disable when tabbar mode is on.  MAYBE: Remove this.
    (with-room-buffer room
      (pcase-let* (((eieio avatar typers name topic) room)
                   (name (when name
                           (propertize name 'face 'font-lock-keyword-face)))
                   (ov (car (ov-in 'matrix-client-prompt)))
                   (typers-string (s-join ", " (cl-loop for user across typers
                                                        collect (matrix-client-displayname-from-user-id room user))))
                   (prompt (if (> (length typers) 0)
                               (concat (propertize (concat "Typing: " typers-string)
                                                   'face 'font-lock-comment-face)
                                       "\n" matrix-client-input-prompt)
                             matrix-client-input-prompt)))
        (ov-set ov 'before-string prompt)
        (setq header-line-format (concat avatar (format "%s: %s" name topic)))))))

;;;;; Timeline

(cl-defmethod matrix-client-ng-m.room.message ((room matrix-room) event)
  "Process m.room.message EVENT in ROOM."
  (pcase-let* (((eieio session) room)
               ((eieio user) session)
               ((map content event_id sender origin_server_ts thumbnail_url url) event)
               ((map body formatted_body msgtype format) content)
               ;; We don't use `matrix-client-event-data-timestamp', because for
               ;; room messages, the origin_server_ts is the actual message time.
               (timestamp (/ origin_server_ts 1000))
               (displayname (matrix-user-displayname room sender))
               (metadata) (msg) (matrix-image-url))
    (when content
      ;; Redacted messages have no content, so we should do nothing for them.
      (setq metadata (format "%s %s> "
                             (format-time-string "[%T]" (seconds-to-time timestamp))
                             displayname))
      (setq message (string-trim
                     ;; Trim messages because HTML ones can have extra newlines
                     (pcase msgtype
                       ("m.emote"
                        (concat "* " body))
                       ((guard (and matrix-client-render-html (string= "org.matrix.custom.html" format)))
                        (with-temp-buffer
                          (insert formatted_body)
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
                        (setq matrix-image-url (matrix-transform-mxc-uri (or url thumbnail_url)))
                        (concat body
                                ": "
                                (matrix-client-linkify-urls matrix-image-url)))
                       (_ (matrix-client-ng-linkify-urls body)))))
      ;; Apply face for own messages
      (let (metadata-face message-face)
        (cond ((equal sender user)
               (setq metadata-face 'matrix-client-own-metadata
                     message-face 'matrix-client-own-messages))
              ((string= msgtype "m.notice")
               (setq metadata-face 'matrix-client-notice-metadata
                     message-face 'matrix-client-notice))
              (t
               (setq metadata-face 'matrix-client-metadata
                     message-face 'default)))
        ;; Use 'append so that link faces are not overridden.
        (add-face-text-property 0 (length metadata) metadata-face 'append metadata)
        (add-face-text-property 0 (length message) message-face 'append message))

      ;; Insert metadata with message and add text properties
      (matrix-client-ng-insert room (propertize (concat metadata message)
                                                'timestamp timestamp
                                                'displayname displayname
                                                'sender sender
                                                'event_id event_id))

      ;; Start image insertion if necessary
      (when matrix-client-ng-show-images
        (cl-loop for url in (-non-nil (append (matrix-client-ng--image-urls message)
                                              (list matrix-image-url)))
                 do (matrix-client-ng-insert-image room event_id url)))

      ;; Move last-seen line if it's our own message
      (when (equal sender user)
        (matrix-client-ng-update-last-seen room))

      ;; Notification
      (unless (equal sender user)
        (matrix-client-notify "m.room.message" event :room room)))))

(defvar matrix-client-ng-show-images nil)

(cl-defmethod matrix-client-ng-insert ((room matrix-room) string)
  "Insert STRING into ROOM's buffer.
STRING should have a `timestamp' text-property."
  (pcase-let* (((eieio extra) room)
               ((eieio buffer) extra)
               (inhibit-read-only t)
               (timestamp (get-text-property 0 'timestamp string)))
    (with-current-buffer buffer
      (cl-loop initially do (progn
                              (goto-char (ov-beg (car (ov-in 'matrix-client-prompt t))))
                              (forward-line -1))
               for buffer-ts = (get-text-property (point) 'timestamp)
               until (when buffer-ts
                       (< buffer-ts timestamp))
               while (when-let (pos (previous-single-property-change (point) 'timestamp))
                       (goto-char pos))
               finally do (when-let (pos (next-single-property-change (point) 'timestamp))
                            (goto-char pos)))
      (insert "\n"
              (propertize string 'read-only t))
      (unless (matrix-client-buffer-visible-p)
        (set-buffer-modified-p t)))))

(cl-defmethod matrix-client-ng-display-name ((room matrix-room))
  "Return display name for ROOM."
  (with-slots (id name aliases) room
    (or name
        (car aliases)
        id)))

(cl-defmethod matrix-client-ng-timeline ((room matrix-room) event)
  "Process EVENT in ROOM."
  (pcase-let* (((map type) event))
    (cl-loop for method = (intern (concat "matrix-client-ng-" (symbol-name type)))
             do (if (functionp method)
                    (funcall method room event)
                  (matrix-warn "Unimplemented method: %s" method)))))

(cl-defmethod matrix-client-ng-setup-room-buffer ((room matrix-room))
  "Prepare and switch to buffer for ROOM-ID, and return room object."
  (with-slots* (((extra) room)
                ((buffer) extra))
    (with-current-buffer buffer
      ;;  (matrix-client-mode)
      (visual-line-mode 1)
      (setq buffer-undo-list t)
      ;; Unset buffer's modified status when it's selected
      (when matrix-client-mark-modified-rooms
        (add-hook 'buffer-list-update-hook #'matrix-client-buffer-list-update-hook 'append 'local))
      (erase-buffer)
      (matrix-client-ng-render-message-line room)
      (matrix-client-ng-insert-last-seen-overlay))
    (switch-to-buffer buffer))
  ;; (set (make-local-variable 'matrix-client-room-connection) con)
  ;; (set (make-local-variable 'matrix-client-room-object) room-obj)
  )

(defun matrix-client-ng-insert-last-seen-overlay ()
  "Insert last-seen overlay into current buffer."
  (when-let ((prompt-ov (car (ov-in 'matrix-client-prompt)))
             (target-pos (1- (ov-beg prompt-ov))))
    (ov target-pos target-pos
        'before-string (concat "\n" (propertize "\n\n" 'face 'matrix-client-last-seen))
        'matrix-client-last-seen t)))

(defmacro with-room-buffer (room &rest body)
  (declare (indent defun))
  `(with-slots* (((extra) room)
                 ((buffer) extra))
     ,@body))

(cl-defmethod matrix-client-ng-update-last-seen ((room matrix-room) &rest _)
  "Move the last-seen overlay to after the last message in ROOM."
  (with-room-buffer room
    (when-let ((prompt-ov (car (ov-in 'matrix-client-prompt)))
               (seen-ov (car (ov-in 'matrix-client-last-seen)))
               (target-pos (1- (ov-beg prompt-ov))))
      (ov-move seen-ov target-pos target-pos))))

(cl-defmethod matrix-client-ng-render-message-line ((room matrix-room))
  "Insert a message input at the end of the buffer."
  (goto-char (point-max))
  (let ((inhibit-read-only t)
        (ov-sticky-front t))
    (insert (propertize "\n" 'read-only t)
            "\n")
    (ov (point) (point)
        'before-string (concat (propertize "\n" 'face '(:height 0.1))
                               matrix-client-input-prompt)
        'matrix-client-prompt t)))

;;;; Update-room-at-once approach

(cl-defmethod matrix-client-ng-update ((room matrix-room))
  "Update ROOM."
  (with-slots* (((extra timeline-new) room)
                ((buffer) extra))
    ;; Make buffer if necessary
    (unless buffer
      (setq buffer (get-buffer-create (matrix-client-ng-display-name room)))
      (matrix-client-ng-setup-room-buffer room))
    ;; Process new events
    (seq-doseq (event timeline-new)
      (pcase-let* (((map data type) event)
                   (method (intern (concat "matrix-client-ng-" type))))
        (if (functionp method)
            (funcall method room event)
          (matrix-warn "Unimplemented method: %s" method))))
    ;; Clear new events
    (matrix-clear-timeline room)
    ;; TODO: Update other room things: header, avatar, typers, topic, name, aliases, etc.
    ))

;;;; Footer

(provide 'matrix-client-ng)

;;; matrix-client-ng.el ends here
