;; Here is a playground for implementing the latest version of the
;; API, r0.2.0.  Confusingly, "v1" of the API is older and more
;; primitive than "r0".  Apparently the API was not considered
;; "released" then, and now it is, so the versions are named with an R
;; instead of a V.

(cl-defmethod matrix-client-sync ((con matrix-client-connection))
  ;; https://matrix.org/docs/spec/client_server/r0.2.0.html#id126
  (with-slots (token) con
    (request (concat matrix-homeserver-base-url "/_matrix/client/r0/sync")
             :params (a-list 'access_token token)
             :error (apply-partially #'matrix-request-error-handler con)
             :parser 'json-read
             :success (apply-partially #'matrix-client-sync-callback con))))

(cl-defmethod matrix-client-sync-callback ((con matrix-client-connection)
                                           &key data error-thrown symbol-status response &allow-other-keys)
  "Process RESPONSE from `matrix-client-sync`."
  (pcase-let* (((map rooms) data)
               ((map join) rooms))
    (--each join
      (matrix-client-sync-room-join con it))))

(cl-defmethod matrix-client-sync-room-join ((con matrix-client-connection) room-join)
  "Process sync join events in ROOM-JOIN.
ROOM-JOIN should be an alist of the room's \"join\" object, as
returned by the Matrix API, not a matrix-client-room object, in
the form (ROOM-ID (ACCOUNT_DATA...) (EPHEMERAL...) ...)."
  (pcase-let* ( ;; room-id will be a symbol instead of a string
               (`(,room-id . ,attrs) room-join)
               ((eieio rooms) con)
               (room-id (symbol-name room-id))
               ;; Use a-get because pcase uses eq, which can't match on string keys.
               (room (a-get rooms room-id))
               ((map ephemeral state timeline) attrs)
               ((map events limited prev_batch) timeline))
    (cl-loop for event across events
             do (matrix-client-event room event))))

(cl-defmethod matrix-client-event ((room matrix-client-room) event)
  "Process EVENT in ROOM."
  (pcase-let* (((map type) event)
               (fn (intern-soft (concat "matrix-client-room-event-" type))))
    (if (functionp fn)
        (funcall fn room event)
      (warn "Unimplemented handler for event: %s" event))))

(cl-defmethod matrix-client-room-event-m.room.message ((room matrix-client-room) event)
  (pcase-let* (((map content event_id origin_server_ts sender) event)
               ((map body formatted_body msgtype format url thumbnail_url) content)
               (timestamp (/ origin_server_ts 1000))
               (display-name (matrix-client-displayname-from-user-id room sender))
               (own-display-name (oref* room :con :username))
               (metadata (format "%s %s> "
                                 (format-time-string "[%T]" (seconds-to-time timestamp))
                                 display-name))
               (metadata-face (pcase display-name
                                (own-display-name 'matrix-client-own-metadata)
                                (_ 'matrix-client-metadata)))
               (message-face (pcase display-name
                               (own-display-name 'matrix-client-own-messages)
                               (_ 'default)))
               (message (when content
                          (string-trim
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
                              (concat body
                                      ": "
                                      (matrix-client-linkify-urls
                                       (matrix-transform-mxc-uri (or url
                                                                     thumbnail_url)))))
                             (t (matrix-client-linkify-urls body)))))))
    ;; Use 'append so that link faces are not overridden.
    (add-face-text-property 0 (length metadata) metadata-face 'append metadata)
    (add-face-text-property 0 (length message) message-face 'append message)
    ;; Insert text with properties
    (matrix-client-insert room (propertize (concat metadata message)
                                           'timestamp timestamp
                                           'display-name display-name
                                           'sender sender
                                           'event_id event_id))
    ;; Notification
    (unless (equal own-display-name display-name)
      (run-hook-with-args 'matrix-client-notify-hook "m.room.message" event
                          :room room))))

;; (cl-defmethod matrix-client-room-event-m.room.message ((room matrix-client-room) event)
;;   ;; Just for testing
;;   (pcase-let* (((eieio id) room))
;;     (warn "EVENT m.room.message in %s: %s" id event)))



(when nil
  ;; Easy way to stop these from being evaluated when the whole buffer
  ;; is.

  (--each matrix-client-connections
    (matrix-client-sync (cdr it))))
