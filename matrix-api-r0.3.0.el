;; Here is a playground for implementing the latest version of the
;; API, r0.2.0.  Confusingly, "v1" of the API is older and more
;; primitive than "r0".  Apparently the API was not considered
;; "released" then, and now it is, so the versions are named with an R
;; instead of a V.

;;; Code:

;;;; Variables

(defvar matrix-log-buffer "*matrix-log*"
  "Name of buffer used by `matrix-log'.")

(defvar matrix-synchronous nil
  "When non-nil, run `matrix-request' requests synchronously.")

;;;; Macros

(defmacro matrix-defclass (name superclasses slots &rest options-and-doc)
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

(cl-defmacro matrix-defcallback (name type docstring &key slots body)
  "Define callback function NAME on TYPE with DOCSTRING and BODY.
This defines a method on a TYPE object, compatible with the
`request' callback API.  The object of TYPE will be available as
TYPE without any `matrix-' prefix.  The method's name will be
`matrix-NAME-callback'. Object SLOTS are made available
automatically with `with-slots'.  Keyword arguments DATA,
ERROR-THROWN, SYMBOL-STATUS, and RESPONSE are defined
automatically, and other keys are allowed."
  (declare (indent defun))
  (let ((name (intern (concat "matrix-" (symbol-name name) "-callback")))
        (instance (intern (nth 1 (s-match (rx "matrix-" (group (1+ anything)))
                                          (symbol-name type))))))
    `(cl-defmethod ,name ((,instance ,type) &key data error-thrown symbol-status response
                          &allow-other-keys)
       (with-slots ,slots ,instance
         ,body))))

;;;; Classes

(matrix-defclass matrix-session ()
  ((user :initarg :user
         :initform nil
         :type string
         :documentation "The fully qualified user ID, e.g. @user:matrix.org.")
   (server :initarg :server
           :initform nil
           :instance-initform (nth 2 (s-match (rx "@" (group (1+ (not (any ":")))) ":" (group (1+ anything)))
                                              user))
           :type string
           :documentation "FQDN of server, e.g. \"matrix.org\" for the official homeserver.
Derived automatically from USER.")
   (api-url-prefix :type string
                   :instance-initform (concat "https://" server "/_matrix/client/r0/")
                   :documentation "URL prefix for API requests.
Derived automatically from server-name and built-in API version.")
   (device-id :initarg :device-id
              ;; FIXME: Does the initform work for this?  When this
              ;; file gets byte-compiled, does it get hard-coded in
              ;; the class definition?  Does this need to be in an
              ;; instance-initform instead?
              :initform (md5 (concat "matrix-client.el" (system-name)))
              :documentation "ID of the client device.")
   (initial-device-display-name
    :initarg :initial-device-display-name
    ;; FIXME: Does the initform work for this?  When this
    ;; file gets byte-compiled, does it get hard-coded in
    ;; the class definition?  Does this need to be in an
    ;; instance-initform instead?
    :initform (concat "matrix-client.el @ " (system-name))
    :type string
    :documentation "A display name to assign to the newly-created device.
Ignored if device_id corresponds to a known device.")
   (access-token :initarg :access-token
                 :initform nil
                 :documentation "API access_token.")
   (txn-id :initarg :txn-id
           :initform 0
           :type integer
           :documentation "Transaction ID.
Defaults to 0 and should be automatically incremented for each request.")
   (rooms :initform nil
          :type list
          :documentation "List of room objects user has joined.")
   (since :initform nil
          :type string
          :documentation "API \"next_batch\" token of the last successful sync request, used for the \"since\" token in the next request.  If a sync is not fully completed, this value will not be updated, so we can try the sync again."))
  :allow-nil-initform t)

(matrix-defclass matrix-room ()
  ((id :documentation "Fully-qualified room ID."
       :type string
       :initform nil)
   (members :documentation "List of room members, as user objects."
            :type list
            :initform nil)
   (state :documentation "Updates to the state, between the time indicated by the since parameter, and the start of the timeline (or all state up to the start of the timeline, if since is not given, or full_state is true)."
          :initform nil)
   (timeline :documentation "List of timeline events."
             :type list
             :initform nil)
   (ephemeral :documentation "The ephemeral events in the room that aren't recorded in the timeline or state of the room. e.g. typing."
              :initform nil)
   (account-data :documentation "The private data that this user has attached to this room."
                 :initform nil)
   (unread-notifications :documentation "Counts of unread notifications for this room."
                         :initform nil))
  :allow-nil-initform t)

;;;; Functions

(cl-defun matrix-log (message &rest args)
  "Log MESSAGE with ARGS to Matrix log buffer.
MESSAGE and ARGS should be a string and list of strings for
`format'."
  (with-current-buffer (get-buffer-create matrix-log-buffer)
    (insert (apply #'format message args) "\n")))

(defun matrix-get (&rest args)
  "Call `matrix-request' with ARGS for a \"GET\" request."
  (apply #'matrix-request args ))

(defun matrix-post (&rest args)
  "Call `matrix-request' with ARGS for a \"POST\" request."
  (nconc args (list :method 'post))
  (apply #'matrix-request args))

;;;; Methods

;;;;; Request

(cl-defmethod matrix-request ((session matrix-session) endpoint data callback
                              &optional &key (method 'get) (error-callback #'matrix-request-error-callback))
  "Make request to ENDPOINT on SESSION with DATA and call CALLBACK on success.
Request is made asynchronously.  METHOD should be a symbol,
`get' (the default) or `post'.  ENDPOINT may be a string or
symbol and should represent the final part of the API
URL (e.g. for \"/_matrix/client/r0/login\", it should be
\"login\".  DATA should be an alist which will be automatically
encoded to JSON.  CALLBACK should be a callback function defined
in accordance with the `request' package's API.  ERROR-CALLBACK,
if set, will be called if the request fails."
  (with-slots (api-url-prefix access-token) session
    (let* ((url (concat api-url-prefix (cl-typecase endpoint
                                         (string endpoint)
                                         (symbol (symbol-name endpoint)))))
           (data (map-put data 'access_token access-token))
           (data (map-filter
                  ;; Remove keys with null values
                  (lambda (k v)
                    v)
                  data))
           (method (upcase (symbol-name method))))
      (matrix-log "REQUEST: %s" (a-list 'method method
                                        'endpoint endpoint
                                        'data data
                                        'callback callback))
      (pcase method
        ("GET" (request url
                        :type method
                        :params data
                        :parser #'json-read
                        :success (apply-partially callback session)
                        :error (apply-partially error-callback session)
                        :sync matrix-synchronous))
        ("POST" (request url
                         :type method
                         :data (json-encode data)
                         :parser #'json-read
                         :success (apply-partially callback session)
                         :error (apply-partially error-callback session)
                         :sync matrix-synchronous))))))

(matrix-defcallback request-error matrix-session
  "Callback function for request error."
  :slots (user)
  :body (let ((msg (format "REQUEST ERROR: %s: %s" user data)))
          (warn msg)
          (matrix-log msg)))

;;;;; Login

(cl-defmethod matrix-login ((session matrix-session) password)
  "Log in to SESSION with PASSWORD.
Session should already have its USER slot set, and optionally its
DEVICE-ID and INITIAL-DEVICE-DISPLAY-NAME."
  (with-slots (user device-id initial-device-display-name) session
    (matrix-post session 'login (a-list 'type "m.login.password"
                                        'user user
                                        'password password
                                        'device_id device-id
                                        'initial_device_display_name initial-device-display-name)
                 #'matrix-login-callback)))

(matrix-defcallback login matrix-session
  "Callback function for successful login.
Set access_token and device_id in session and start initial
sync."
  :slots (access-token device-id)
  :body (pcase-let* (((map access_token device_id) data))
          (matrix-log "LOGIN CALLBACK: %s" data)
          (setq access-token access_token
                device-id device_id)
          (matrix-sync session)))

;;;;; Sync

(cl-defmethod matrix-sync ((session matrix-session) &key full-state set-presence timeout)
  ;; https://matrix.org/docs/spec/client_server/r0.2.0.html#id126
  (with-slots (access-token since) session
    (matrix-get session 'sync
                (a-list 'since since
                        'full_state full-state
                        'set_presence set-presence
                        'timeout timeout)
                #'matrix-sync-callback)))

(matrix-defcallback sync matrix-session
  "Callback function for successful sync request."
  ;; https://matrix.org/docs/spec/client_server/r0.3.0.html#id167
  :slots (rooms since)
  :body (let (failure)
          (matrix-log "SYNC CALLBACK: %s" data)
          (--each '(rooms presence account_data to_device device_lists)
            (unless failure
              (let ((method-name (intern (concat "matrix-sync-" (symbol-name it)))))
                (if (functionp method-name)
                    (unless (funcall method-name session (a-get data it))
                      (setq failure t))
                  (warn "Unimplemented method: %s" method-name)))))
          (unless failure
            (setq since (a-get data next_batch)))))

(cl-defmethod matrix-sync-rooms ((session matrix-session) rooms)
  "Process ROOMS from sync response on SESSION."
  ;; https://matrix.org/docs/spec/client_server/r0.3.0.html#id167
  (--each rooms
    (pcase it
      (`(join _) (matrix-sync-join session it))
      (`(invite _) (matrix-log "Would process room invites: %s" it))
      (`(leave _) (matrix-log "Would process room leaves: %s" it)))))

(cl-defmethod matrix-sync-join ((session matrix-session) join)
  "Sync JOIN, a list of joined rooms, on SESSION."
  ;; https://matrix.org/docs/spec/client_server/r0.3.0.html#id167
  (with-slots (rooms) session
    (--each join
      (pcase-let* ((`(,joined-room-id . ,joined-room) it)
                   (params '(state timeline ephemeral account_data unread_notifications))
                   (room-obj (or (cl-loop for room in rooms
                                          when (equal (oref room id) joined-room-id)
                                          return it)
                                 ;; Make new room
                                 (matrix-room :id room-id)))
                   (failure))
        (--each params
          (unless failure
            (let ((method-name (intern (concat "matrix-sync-") (symbol-name it))))
              (if (functionp method-name)
                  (unless (funcall method-name room (a-get joined-room it))
                    (setq failure t))
                (warn "Unimplemented method: %s" method-name)))))
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

(cl-defmethod matrix-sync-account_data ((room matrix-room) account-data)
  "Sync ACCOUNT-DATA in ROOM."
  (pcase-let (((map events) account-data))
    (--each events
      (matrix-log "Would process account-data event in %s: " room it))))

(cl-defmethod matrix-sync-unread_notifications ((room matrix-room) unread-notifications)
  "Sync UNREAD-NOTIFICATIONS in ROOM."
  (pcase-let (((map highlight_count notification_count) unread-notifications))
    (matrix-log "Would process highlight_count in %s: " room highlight_count)
    (matrix-log "Would process notification_count in %s: " room notification_count)))

;;; Footer

(provide 'matrix-api-r0.3.0)
