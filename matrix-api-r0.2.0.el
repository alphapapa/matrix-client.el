;; Here is a playground for implementing the latest version of the
;; API, r0.2.0.  Confusingly, "v1" of the API is older and more
;; primitive than "r0".  Apparently the API was not considered
;; "released" then, and now it is, so the versions are named with an R
;; instead of a V.

(defmacro matrix-client-defclass (name superclasses slots &rest options-and-doc)
  (declare (indent defun))
  (let* ((slot-inits (-non-nil (--map (let ((name (car it))
                                            (initer (plist-get (cdr it) :instance-initform)))
                                        (when initer
                                          (list 'setq name initer)))
                                      slots)))
         (slot-names (mapcar #'car slots))
         (around-fn-name (intern (concat (symbol-name name) "-initialize")))
         (docstring (format "Inititalize instance of %s." name)))
    `(progn
       (defclass ,name ,superclasses ,slots ,@options-and-doc)
       (when (> (length ',slot-inits) 0)
         (cl-defmethod initialize-instance :after ((this ,name) &rest _)
                       ,docstring
                       (with-slots ,slot-names this
                         ,@slot-inits))))))

(matrix-client-defclass matrix-session ()
  ((server :initarg :server
           :initform "matrix.org"
           :type string
           :allow-nil-initform t
           :documentation "FQDN of server, e.g. \"matrix.org\" for the official homeserver.  Defaults to matrix.org.")
   (api-url-prefix :type string
                   :instance-initform (concat "https://" server "/_matrix/client/r0/")
                   :documentation "URL prefix for API requests.  Derived automatically from server-name and built-in API version.")
   (user :initarg :user
         :initform nil
         :type string
         :allow-nil-initform t
         :documentation "The fully qualified user ID, e.g. @user:matrix.org.")
   (device-id :initarg :device-id
              :initform nil
              :allow-nil-initform t
              :documentation "ID of the client device.")
   (initial-device-display-name
    :initarg :initial-device-display-name
    :initform (concat "matrix-client.el @ " (system-name))
    :type string
    :documentation "A display name to assign to the newly-created device. Ignored if device_id corresponds to a known device.")
   (access-token :initarg :access-token
                 :initform nil
                 :allow-nil-initform t
                 :documentation "API access_token.")
   (txn-id :initarg :txn-id
           :initform 0
           :type integer
           :documentation "Transaction ID.  Defaults to 0 and should be automatically incremented for each request.")
   (rooms :initform nil
          :type list
          :documentation "List of room objects user has joined.")
   (last-since :initform nil
               :type string
               :documentation "API \"since\" token of the last successful sync request.  If a sync is not fully completed, this value will not be updated, so we can try the sync again.")))

(cl-defmethod matrix-session-request ((session matrix-session) endpoint data callback
                                      &optional &key (error-callback #'matrix-session-error-callback))
  "Make request to ENDPOINT on SESSION with DATA and call CALLBACK on success.
Request is made asynchronously.  ENDPOINT may be a string or
symbol and should represent the final part of the API
URL (e.g. for \"/_matrix/client/r0/login\", it should be
\"login\".  DATA should be an alist which will be automatically
encoded to JSON.  CALLBACK should be a callback function defined
in accordance with the `request' package's API.  ERROR-CALLBACK,
if set, will be called if the request fails."
  (with-slots (api-url-prefix access-token) session
    (let ((url (concat api-url-prefix (cl-typecase endpoint
                                        (string endpoint)
                                        (symbol (symbol-name endpoint)))))
          (data (map-put data 'access_token access-token)))
      (request url
               :params data
               :parser #'json-read
               :success (apply-partially callback session)
               :error (apply-partially error-callback session)))))

(cl-defmethod matrix-session-error-callback ((session matrix-session) &rest args)
  "Generic error callback."
  (error "Matrix request failed: %s" args))

(cl-defmethod matrix-client-login ((session matrix-session) password)
  "Log in to SESSION with PASSWORD.
Session should already have its USER slot set, and optionally its
DEVICE-ID and INITIAL-DEVICE-DISPLAY-NAME."
  (matrix-session-request session 'login (a-list 'password password)
                          #'matrix-client-login-callback))

(cl-defmacro matrix-session-defcallback (name docstring &key slots body)
  "Define callback function NAME with DOCSTRING and BODY.
This defines a method on a `matrix-session' object, compatible
with the `request' callback API.  The method's name will be
`matrix-session-NAME-callback'.  The `matrix-session' object is
available as SESSION.  Session SLOTS are made available
automatically with `with-slots'.  Keyword arguments DATA,
ERROR-THROWN, SYMBOL-STATUS, and RESPONSE are defined
automatically, and other keys are allowed."
  (declare (indent defun))
  (let ((name (intern (concat "matrix-session-" (symbol-name name) "-callback"))))
    `(cl-defmethod ,name ((session matrix-session) &key data error-thrown symbol-status response
                          &allow-other-keys)
       (with-slots ,slots session
         ,body))))

(matrix-session-defcallback login
  "Callback function for successful login.
Set access_token and device_id in session and start initial
sync."
  :slots '(access-token device-id)
  :body (pcase-let* (((map access_token device_id) data))
          (setq access-token access_token
                device-id device_id)
          (matrix-session-sync session)))

(cl-defmethod matrix-client-sync ((con matrix-client-connection) &key full-state set-presence timeout)
  ;; https://matrix.org/docs/spec/client_server/r0.2.0.html#id126
  (with-slots (token) con
    (request (concat matrix-homeserver-base-url "/_matrix/client/r0/sync")
             ;; FIXME: Do we need to remove keys of null values?
             :params (a-list 'access_token token
                             'full_state full-state
                             'set_presence set-presence
                             'timeout timeout)
             :error (apply-partially #'matrix-request-error-handler con)
             :parser 'json-read
             :success (apply-partially #'matrix-client-sync-callback con))))



(matrix-session-defcallback sync
  "Callback function for successful sync request."
  ;; https://matrix.org/docs/spec/client_server/r0.3.0.html#id167
  :slots '(rooms last-since)
  :body (let (failure)
          (--each '(next_batch rooms presence account_data to_device device_lists)
            (unless failure
              (if-let ((method-name (intern-soft (concat "matrix-sync-") (symbol-name it))))
                  (unless (funcall method-name session (a-get data param))
                    (setq failure t))
                (warn "Unimplemented method: %s" method-name))))
          (unless failure
            (setq last-since since))))

(cl-defmethod matrix-session-sync-rooms ((session matrix-session) rooms)
  "Process ROOMS from sync response on SESSION."
  ;; https://matrix.org/docs/spec/client_server/r0.3.0.html#id167
  (--each rooms
    (pcase it
      (`(join _) (matrix-sync-join it))
      (`(invite _) (matrix-log "Would process room invites: %s" it))
      (`(leave _) (matrix-log "Would process room leaves: %s" it)))))

(cl-defmethod matrix-sync-join ((session matrix-session) join)
  "Sync JOIN, a list of joined rooms, on SESSION."
  ;; https://matrix.org/docs/spec/client_server/r0.3.0.html#id167
  (with-slots (rooms) session
    (--each join
      (pcase-let* ((`(,joined-room-id . ,joined-room) it)
                   (params '(state timeline ephemeral account_data unread_notifications))
                   ;;  ((map state timeline ephemeral account_data unread_notifications) joined-room)
                   (room-obj (or (cl-loop for room in rooms
                                          when (equal (oref room id) joined-room-id)
                                          return it)
                                 ;; Make new room
                                 (matrix-room :id room-id)))
                   (failure))
        (--each params
          (unless failure
            (if-let ((method-name (intern-soft (concat "matrix-sync-") (symbol-name it))))
                (unless (funcall method-name room (a-get joined-room param))
                  (setq failure t))
              (warn "Unimplemented method: %s" method-name))))
        (unless failure
          (setq last-since since))))))

(cl-defmethod matrix-sync-state ((room matrix-room) state)
  "Sync STATE in ROOM."
  (pcase-let (((map events) state))
    (--each events
      (matrix-log "Would process state event in %s: " room it))))

(cl-defmethod matrix-sync-timeline ((room matrix-room) timeline)
  "Sync TIMELINE in ROOM."
  (pcase-let (((map events limited prev_batch) timeline))
    (--each events
      (matrix-log "Would process timeline event in %s: " room it))))

(cl-defmethod matrix-sync-ephemeral ((room matrix-room) ephemeral)
  "Sync EPHEMERAL in ROOM."
  (pcase-let (((map events) ephemeral))
    (--each events
      (matrix-log "Would process ephemeral event in %s: " room it))))

(cl-defmethod matrix-sync-account-data ((room matrix-room) account-data)
  "Sync ACCOUNT-DATA in ROOM."
  (pcase-let (((map events) account-data))
    (--each events
      (matrix-log "Would process account-data event in %s: " room it))))

(cl-defmethod matrix-sync-unread-notifications ((room matrix-room) unread-notifications)
  "Sync UNREAD-NOTIFICATIONS in ROOM."
  (pcase-let (((map highlight_count notification_count) unread-notifications))
    (matrix-log "Would process highlight_count in %s: " room highlight_count)
    (matrix-log "Would process notification_count in %s: " room notification_count)))

(defvar matrix-log-buffer "*matrix-log*"
  "Name of buffer used by `matrix-log'.")

(cl-defun matrix-log (message &rest args)
  "Log MESSAGE with ARGS to Matrix log buffer.
MESSAGE and ARGS should be a string and list of strings for
`format'."
  (with-current-buffer matrix-log-buffer
    (insert (apply #'format message args) "\n")))


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
