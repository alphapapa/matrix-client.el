;; Here is a playground for implementing the latest version of the
;; API, r0.3.0.  Confusingly, "v1" of the API is older and more
;; primitive than "r0".  Apparently the API was not considered
;; "released" then, and now it is, so the versions are named with an R
;; instead of a V.

;; TODO: Consider let-binding `json-array-type' to `list'.  I'm not
;; sure there's any advantage to using vectors, and it makes the code
;; more error-prone because some things are lists and some vectors.

;;; Code:

;;;; Requirements

;; Built-in
(require 'cl-lib)
(require 'eieio)
(require 'map)
(require 'seq)

;; MELPA
(require 'a)
(require 'dash)
(require 'ht)
(require 'json)
(require 'request)
(require 's)

;; Local
(require 'matrix-macros)

;;;; Variables

(defvar matrix-log-buffer "*matrix-log*"
  "Name of buffer used by `matrix-log'.")

(defvar matrix-synchronous nil
  "When non-nil, run `matrix-request' requests synchronously.")

;;;; Macros

(defmacro matrix-defclass (name superclasses slots &rest options-and-doc)
  "Identical to `defclass', except...
adds nil initforms.  Also adds optional `:instance-initform'
argument to each slot.  This should be a sexp that will be
evaluated in the context of the object's slots (using
`with-slots') when it is initialized (like Python's __init__
method)."
  (declare (indent defun))
  (let* ((slot-inits (-non-nil (--map (let ((name (car it))
                                            (initer (plist-get (cdr it) :instance-initform)))
                                        (when initer
                                          (list 'setq name initer)))
                                      slots)))
         (slot-names (mapcar #'car slots))
         (around-fn-name (intern (concat (symbol-name name) "-initialize")))
         (docstring (format "Inititalize instance of %s." name)))
    ;; Add nil initforms
    (cl-loop for (slot . attrs) in slots
             unless (plist-get attrs :initform)
             do (nconc attrs (list :initform nil)))
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

;;;; Functions

(defun funcall-when (fn &rest args)
  "If FN is a function, return result of applying ARGS to it, otherwise nil."
  (let ((fn (intern-soft fn)))
    (when (functionp fn)
      (apply fn args))))

(defmacro funcall-if (fn-name args else)
  "If FN-NAME is a function, return result of applying ARGS to it, otherwise ELSE.
FN-NAME should be a string, and is available in the ELSE form as `fn-name'."
  (declare (debug (form listp form)))
  `(let ((fn-name ,fn-name)
         (fn (intern-soft ,fn-name)))
     (if (functionp fn)
         (apply fn ,args)
       ,else)))
(put 'funcall-if 'lisp-indent-function 2)

;;;; Classes

(matrix-defclass matrix-session ()
  ((user :initarg :user
         :type string
         :documentation "The fully qualified user ID, e.g. @user:matrix.org.")
   (server :initarg :server
           :instance-initform (nth 2 (s-match (rx "@" (group (1+ (not (any ":"))))
                                                  ":" (group (1+ anything)))
                                              user))
           :type string
           :documentation "FQDN of server, e.g. \"matrix.org\" for the official homeserver.  Derived automatically from USER.")
   (api-url-prefix :type string
                   :instance-initform (concat "https://" server "/_matrix/client/r0/")
                   :documentation "URL prefix for API requests.  Derived automatically from server-name and built-in API version.")
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
    :documentation "A display name to assign to the newly-created device.  Ignored if device_id corresponds to a known device.")
   (access-token :initarg :access-token
                 :documentation "API access_token.")
   (txn-id :initarg :txn-id
           :initform 0
           :type integer
           :documentation "Transaction ID.  Defaults to 0 and should be automatically incremented for each request.")
   (rooms :initarg :rooms
          :type list
          :documentation "List of room objects user has joined.")
   (account-data :documentation "The private data that this user has attached to this account.")
   (followed-users :initform (ht)
                   :initarg :followed-users
                   :type hash-table
                   :documentation "Hash table of user IDs whose presence this user wants to follow.")
   (next-batch :type string
               :documentation "The batch token to supply in the since param of the next /sync request.")
   (extra :initarg :extra
          :documentation "Reserved for users of the library, who may store whatever they want here."))
  :allow-nil-initform t)

;;;;; Room

(matrix-defclass matrix-room ()
  ((session :initarg :session
            :type matrix-session)
   (id :documentation "Fully-qualified room ID."
       :initarg :id
       :type string)
   (avatar :initarg :avatar)
   (typers :initarg :typers
           ;; MAYBE: Not sure if we need this, haven't gotten this far yet.
           )
   (name :initarg :name
         :type string)
   (topic :initarg :topic
          :type string)
   (aliases :initarg :aliases)
   (members :documentation "List of room members, as user objects."
            :type list)
   (state :documentation "Updates to the state, between the time indicated by the since parameter, and the start of the timeline (or all state up to the start of the timeline, if since is not given, or full_state is true).")
   (timeline :documentation "List of timeline events."
             :type list)
   (timeline-new :documentation "List of new timeline events.  Clients may clear this list by calling `matrix-clear-timeline'."
                 :type list)
   (prev-batch :documentation "A token that can be supplied to to the from parameter of the rooms/{roomId}/messages endpoint.")
   (last-full-sync :documentation "The oldest \"since\" token for which the room has been synced completely.")
   (ephemeral :documentation "The ephemeral events in the room that aren't recorded in the timeline or state of the room. e.g. typing.")
   (account-data :documentation "The private data that this user has attached to this room.")
   (unread-notifications :documentation "Counts of unread notifications for this room.")
   (hook :initarg :hook
         :documentation "List of functions called when room is updated.  Function is called with one argument, this room object.")
   (extra :initarg :extra
          ;; FIXME: Need clean way to do this.
          :initform (matrix-room-extra)
          :documentation "Reserved for users of the library, who may store whatever they want here."))
  :allow-nil-initform t)

(cl-defmethod matrix-user-displayname ((room matrix-room) user-id)
  "Return display name for USER-ID in ROOM."
  (pcase-let* (((eieio members) room)
               ((map (user-id user)) members)
               ((map displayname) user))
    (or displayname user-id)))

;;;; Functions

(defun matrix-log (message &rest args)
  "Log MESSAGE with ARGS to Matrix log buffer and return non-nil.
MESSAGE and ARGS should be a string and list of strings for
`format'."
  (with-current-buffer (get-buffer-create matrix-log-buffer)
    (insert (apply #'format message args) "\n")
    ;; Returning t is more convenient than nil, which is returned by `message'.
    t))

(defun matrix-warn (message &rest args)
  "Log MESSAGE with ARGS to Matrix log buffer and signal warning with same MESSAGE.
MESSAGE and ARGS should be a string and list of strings for
`format'."
  (apply #'matrix-log message args)
  (apply #'warn message args))

(defun matrix-get (&rest args)
  "Call `matrix-request' with ARGS for a \"GET\" request."
  (apply #'matrix-request args ))

(defun matrix-post (&rest args)
  "Call `matrix-request' with ARGS for a \"POST\" request."
  (nconc args (list :method 'post))
  (apply #'matrix-request args))

(defun matrix-put (&rest args)
  "Call `matrix-request' with ARGS for a \"PUT\" request."
  (nconc args (list :method 'put))
  (apply #'matrix-request args))

;;;; Methods

;;;;; Request

(cl-defmethod matrix-request ((session matrix-session) endpoint data callback
                              &optional &key (method 'get) (error-callback #'matrix-request-error-callback)
                              complete-callback timeout)
  "Make request to ENDPOINT on SESSION with DATA and call CALLBACK on success.
Request is made asynchronously.  METHOD should be a symbol,
`get' (the default) or `post'.  ENDPOINT may be a string or
symbol and should represent the final part of the API
URL (e.g. for \"/_matrix/client/r0/login\", it should be
\"login\".  DATA should be an alist which will be automatically
encoded to JSON.  CALLBACK should be a method specialized on
`matrix-session', whose subsequent arguments are defined in
accordance with the `request' package's API.  ERROR-CALLBACK, if
set, will be called if the request fails."

  ;; TODO: Add general completion callback that retries requests if they timeout.  e.g. if a
  ;; message-send request times out, we should retry it at least once, and then give an error.
  ;; We should check the API docs to see if there's a recommended timeout value for requests
  ;; like that.

  ;; TODO: Use request's :status-code argument to handle error responses more precisely.

  (with-slots (api-url-prefix access-token) session
    (let* ((url (url-encode-url
                 (concat api-url-prefix (cl-typecase endpoint
                                          (string endpoint)
                                          (symbol (symbol-name endpoint))))))
           (data (map-filter
                  ;; Remove keys with null values
                  (lambda (k v)
                    v)
                  data))
           (callback (cl-typecase callback
                       ;; If callback is a symbol, apply session to
                       ;; it.  If it's an already-partially-applied
                       ;; function, use it as-is.
                       ;; FIXME: Add to docstring.
                       (symbolp (apply-partially callback session))
                       (t callback)))
           (complete-callback (pcase complete-callback
                                ;; If complete-callback is a symbol, apply session to
                                ;; it.  If it's an already-partially-applied
                                ;; function, use it as-is.
                                ;; FIXME: Add to docstring.
                                (`nil nil)
                                ((pred symbolp) (apply-partially complete-callback session))
                                (_ complete-callback)))
           (method (upcase (symbol-name method)))
           (request-log-level 'debug))
      (matrix-log "REQUEST: %s" (a-list 'url url
                                        'method method
                                        'data data
                                        'callback callback
                                        'timeout timeout))
      (pcase method
        ("GET" (request url
                        :type method
                        :headers (a-list 'Authorization (format "Bearer %s" access-token))
                        :params data
                        :parser #'json-read
                        :success callback
                        :error (apply-partially error-callback session)
                        :complete complete-callback
                        :timeout timeout
                        :sync matrix-synchronous))
        ((or "POST" "PUT") (request url
                                    :type method
                                    :headers (a-list 'Content-Type "application/json"
                                                     'Authorization (format "Bearer %s" access-token))
                                    :data (json-encode data)
                                    :parser #'json-read
                                    :success callback
                                    :error (apply-partially error-callback session)
                                    :complete complete-callback
                                    :timeout timeout
                                    :sync matrix-synchronous))))))

(matrix-defcallback request-error matrix-session
  "Callback function for request error."
  :slots (user)
  :body (matrix-warn "REQUEST ERROR:%s  RESPONSE:%s" error-thrown response))

;;;;; Login/logout

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
Set access_token and device_id in session."
  :slots (access-token device-id)
  :body (pcase-let* (((map access_token device_id) data))
          (setq access-token access_token
                device-id device_id)
          (run-hook-with-args 'matrix-login-hook session)))

(cl-defmethod matrix-logout ((session matrix-session))
  "Log out of SESSION."
  (with-slots (user device-id initial-device-display-name) session
    (matrix-post session 'logout nil
                 #'matrix-logout-callback)))

(matrix-defcallback logout matrix-session
  "Callback function for successful logout.
Unset access_token and device_id in session."
  :slots (access-token device-id)
  ;; TODO: Do we need to set the device_id to nill?
  :body (setq access-token nil
              device-id nil))

;;;;; Sync

(cl-defmethod matrix-sync ((session matrix-session) &key full-state set-presence (timeout 30))
  "Start making /sync API requests.
Every TIMEOUT seconds, the server should return any outstanding
requests, and we make a new request."
  ;; https://matrix.org/docs/spec/client_server/r0.2.0.html#id126

  ;; TODO: Probably should add a watchdog in case the server drops the connection and doesn't
  ;; actually return when timed-out.  We should be able to check the `symbol-status' arg to the
  ;; callback, and if it's `timeout' or `abort', we call /sync again.  That would let us avoid
  ;; using a timer.  To do this, we also need to set the timeout arg to `request' to, say, a
  ;; few seconds more than the timeout sent in the API request.

  ;; MAYBE: Don't make new /sync request if timeout is 0/nil.  It would have to be passed
  ;; through to the callback.

  ;; MAYBE: Can we just set the :complete arg to `request' to `matrix-sync' so that it will be
  ;; called repeatedly?  Or should we call `matrix-sync' again in the success callback?  Do we
  ;; want to stop syncing if there's an error?

  ;; NOTE: I guess we should stop syncing if there's an error, because otherwise, if next-batch
  ;; doesn't get updated, it will call sync repeatedly to get the same batch of data that's
  ;; failing, and likely enter an infinite loop (an asynchronous one, at least).  So we'll call
  ;; `matrix-sync' again in the success callback.

  ;; FIXME: Need to handle gaps according to the API: "Normally, all new events which are
  ;; visible to the client will appear in the response to the /sync API. However, if a large
  ;; number of events arrive between calls to /sync, a \"limited\" timeline is returned,
  ;; containing only the most recent message events. A state \"delta\" is also returned,
  ;; summarising any state changes in the omitted part of the timeline. The client may
  ;; therefore end up with \"gaps\" in its knowledge of the message timeline. The client can
  ;; fill these gaps using the /rooms/<room_id>/messages API. This situation looks like this:"

  ;; So IIUC, we need to see if "limited" is true FOR EACH ROOM, and if so, get the
  ;; "prev_batch" token FOR THAT ROOM and call `matrix-messages' with it.  But it seems that we
  ;; also need to call it with an "end" token, which I guess should be the "next_batch" token
  ;; from the last non-limited /sync request (which starts to get confusing).  The API docs
  ;; aren't especially clear here.  NOTE: I guess this will be difficult or impossible to test,
  ;; because I don't know how often the server returns limited timelines (i.e. how busy a room
  ;; has to be), or how we could make it do so, because there is no "limit" param for /sync.

  ;; I think what I will do for now is just raise a warning if "limited" is ever set.

  ;; The /messages call does NOT return a "limited", so I guess there is no way to know when
  ;; you've retrieved all of the messages that /sync WOULD have sent you if there were no
  ;; limit.  So I guess we have to decide for ourselves how many messages we want to show on
  ;; initial sync, and fetch backward until we have that many--unless, of course, we hit the
  ;; beginning of the room, in which case I guess the start/end tokens from /messages will be
  ;; the same...?  Or we'll receive fewer messages than the limit...?

  (with-slots (access-token next-batch) session
    (matrix-get session 'sync (a-list 'since next-batch
                                      'full_state full-state
                                      'set_presence set-presence
                                      ;; Convert timeout to milliseconds
                                      'timeout (* timeout 1000))
                #'matrix-sync-callback
                :complete-callback #'matrix-sync-complete-callback
                ;; Add 5 seconds to timeout to give server a bit of grace period before we
                ;; consider it unresponsive.
                :timeout (+ timeout 5))))

(matrix-defcallback sync matrix-session
  "Callback function for successful sync request."
  ;; https://matrix.org/docs/spec/client_server/r0.3.0.html#id167
  :slots (rooms next-batch)
  :body (cl-loop for param in '(rooms presence account_data to_device device_lists)
                 for method = (intern (concat "matrix-sync-" (symbol-name param)))
                 ;; Assume that methods called will signal errors if anything goes wrong, so
                 ;; ignore return values.
                 do (if (functionp method)
                        (funcall method session (a-get data param))
                      (matrix-warn "Unimplemented API method: %s" fn-name))
                 finally do (setq next-batch (a-get data 'next_batch))))

(matrix-defcallback sync-complete matrix-session
  "Completion callback function for sync requests.
If sync was successful or timed-out, make a new sync request.  If
SESSION has no access token, consider the session logged-out."
  :slots (access-token)
  :body ;; (pcase symbol-status
  ;;   ('success
  ;;    (matrix-log "SYNC SUCCESS.")
  ;;    (unless matrix-synchronous
  ;;      ;; Call self again to wait for more data.  But don't do this if
  ;;      ;; `matrix-synchronous' is set, which would cause an infinite
  ;;      ;; loop.  It should only be set when testing, in which case we
  ;;      ;; sync manually.
  ;;      (matrix-log "NOT SYNCHRONOUS")
  ;;      (when access-token
  ;;        (matrix-log "POLLING...")
  ;;        (matrix-sync session))))
  ;;   ('timeout
  ;;    (matrix-log "SYNC TIMED OUT.")
  ;;    (unless matrix-synchronous
  ;;      ;; Call self again to wait for more data.  But don't do this if
  ;;      ;; `matrix-synchronous' is set, which would cause an infinite
  ;;      ;; loop.  It should only be set when testing, in which case we
  ;;      ;; sync manually.
  ;;      (matrix-log "NOT SYNCHRONOUS")
  ;;      (when access-token
  ;;        (matrix-log "POLLING...")
  ;;        (matrix-sync session))))
  ;;   (_ (matrix-warn "SYNC FAILED: %s  NOT STARTING NEW SYNC REQUEST.  API SHOULD BE CONSIDERED DISCONNECTED."
  ;;                   (upcase (symbol-name symbol-status)))))
  (pcase symbol-status
    ('success
     (matrix-log "SYNC SUCCESS.")
     (if matrix-synchronous
         (matrix-log "SYNCHRONOUS: NOT POLLING")
       (if access-token
           (progn
             (matrix-log "POLLING...")
             (matrix-sync session))
         (matrix-log "NO ACCESS TOKEN: NOT POLLING"))))
    ('timeout
     (matrix-log "SYNC TIMED OUT.")
     (if matrix-synchronous
         (matrix-log "SYNCHRONOUS: NOT POLLING")
       (if access-token
           (progn
             (matrix-log "POLLING...")
             (matrix-sync session))
         (matrix-log "NO ACCESS TOKEN: NOT POLLING"))))
    (_ (matrix-warn "SYNC FAILED: %s  NOT STARTING NEW SYNC REQUEST.  API SHOULD BE CONSIDERED DISCONNECTED."
                    (upcase (symbol-name symbol-status))))))

(cl-defmethod matrix-sync-presence ((session matrix-session) state-changes)
  "Process presence STATE-CHANGES."
  ;; TODO: Test this.
  ;; https://matrix.org/docs/spec/client_server/r0.3.0.html#id294
  (with-slots (followed-users) session
    (seq-doseq (change (a-get state-changes 'events))
      (pcase-let* (((map content) change)
                   ((map avatar_url currently_active last_active_ago presence user_id) content))
        (ht-set followed-users user_id (a-list 'avatar_url avatar_url
                                               'currently_active currently_active
                                               'last_active_ago last_active_ago
                                               'presence presence))))))

(cl-defmethod matrix-sync-rooms ((session matrix-session) rooms)
  "Process ROOMS from sync response on SESSION."
  ;; https://matrix.org/docs/spec/client_server/r0.3.0.html#id167
  (cl-loop for room in rooms
           do (progn
                (pcase room
                  (`(join . ,_) (matrix-sync-join session room))
                  (`(invite .  ,_) (matrix-log "Would process room invites: %s" room))
                  (`(leave . ,_) (matrix-log "Would process room leaves: %s" room))))))

(cl-defmethod matrix-sync-join ((session matrix-session) join)
  "Sync JOIN, a list of joined rooms, on SESSION."
  ;; https://matrix.org/docs/spec/client_server/r0.3.0.html#id167
  (with-slots (rooms) session
    (cl-loop for it in (cdr join)
             always (pcase-let* ((`(,joined-room-id . ,joined-room) it)
                                 ;; Room IDs are decoded from JSON as symbols, so we convert to strings.
                                 (room-id (symbol-name joined-room-id))
                                 (params '(state timeline ephemeral account_data unread_notifications))
                                 (room (or (--first (equal (oref it id) room-id)
                                                    rooms)
                                           ;; Make and return new room
                                           (car (push (matrix-room :session session
                                                                   :id room-id)
                                                      rooms)))))
                      (cl-loop for param in params
                               ;; If the event array is empty, the function will be
                               ;; called anyway, so ignore its return value.
                               do (funcall-if (concat "matrix-sync-" (symbol-name param))
                                      (list room (a-get joined-room param))
                                    (matrix-warn "Unimplemented API method: %s" fn-name))
                               ;; Always return t for now, so that we think the sync succeeded
                               ;; and we can set next_batch in `matrix-sync-callback'.
                               finally return t)
                      ;; Run client hooks
                      (run-hook-with-args 'matrix-room-update-hook room)
                      t))))

(cl-defmethod matrix-sync-state ((room matrix-room) data)
  "Process state DATA in ROOM."
  (with-slots (state) room
    (pcase-let (((map events) data))
      ;; events is an array, not a list, so we can't use --each.
      (seq-doseq (event events)
        (push event state)
        (matrix-event room event)))))

;; (defvar matrix-sync-timeline-hook nil
;;   "List of functions called for new timeline events.
;; Each function is called with ROOM and EVENT.")

(cl-defmethod matrix-sync-timeline ((room matrix-room) data)
  "Sync timeline DATA in ROOM."
  (with-slots* (((id session timeline timeline-new prev-batch last-full-sync) room)
                ((next-batch) session))
    (pcase-let (((map events limited prev_batch) data))
      (seq-doseq (event events)
        (push event timeline)
        (push event timeline-new)
        ;; Run API handler for event.
        (matrix-event room event))
      (setq prev-batch prev_batch)
      (if (and (not (equal limited :json-false)) last-full-sync)
          ;; Timeline is limited and we have a token to fill to: fill the gap.  If
          ;; `last-full-sync' is nil, this should mean that we are doing an initial sync, and
          ;; since we have no previous "since" token to fetch up to, we do not bother to fetch
          ;; more messages, even if the timeline is limited.
          ;; MAYBE: Add setting for minimum number of events/messages to initially fetch.
          (progn
            (matrix-warn "ROOM %s TIMELINE WAS LIMITED: %s.  Trying to fill gap..." id data)
            (matrix-messages room))
        ;; Timeline is not limited: save the not-yet-updated next-batch token.  If the next
        ;; timeline is limited, we use this token to know when we have filled the timeline gap.
        (matrix-log "ROOM %s FULLY SYNCED." id)
        (setq last-full-sync next-batch)))))

(cl-defmethod matrix-event ((room matrix-room) event)
  "Process EVENT in ROOM."
  (pcase-let* (((map type) event))
    (funcall-if (concat "matrix-event-" type)
        (list room event)
      (matrix-log "Unimplemented handler for event %s in room %s." type (oref room id)))))

(cl-defmethod matrix-event-m.room.member ((room matrix-room) event)
  "Process m.room.member EVENT in ROOM."
  (with-slots (members id) room
    (matrix-log "m.room.member for ROOM:%s  EVENT:%s" id event)

    (pcase-let* (((map ('state_key user-id) content) event)
                 ((map membership displayname avatar_url) content))
      (pcase membership
        ;; TODO: Support all membership changes: invite, join, knock, leave, ban.
        ("join" (map-put members user-id (a-list 'displayname displayname
                                                 'avatar-url avatar_url)))
        ("leave" (setq members (map-delete members user-id)))))
    ;; FIXME: Don't think we need this hook, the client can just process the event from timeline-new.
    (run-hook-with-args 'matrix-event-m.room.member-hook room event)))

(cl-defmethod matrix-event-m.room.name ((room matrix-room) event)
  "Process m.room.name EVENT in ROOM."
  (with-slots (name) room
    (setq name (a-get* event 'content 'name))
    (run-hook-with-args 'matrix-room-metadata-hook room)))

(defvar matrix-room-metadata-hook nil
  "List of functions called when a room's metadata is updated.
Each function is called with one argument, the room object that
was updated.")

(defvar matrix-room-update-hook nil
  ;; FIXME: Rename to matrix-room-timeline-hook
  "List of functions called when a room's timeline is updated.
Each function is called with one argument, the room object that
was updated.")



(cl-defmethod matrix-clear-timeline ((room matrix-room))
  "Clear ROOM's `timeline-new' list."
  (with-slots (timeline-new) room
    (setq timeline-new nil)))

(cl-defmethod matrix-messages ((room matrix-room)
                               &key (direction "b") (limit 100))
  "Request messages for ROOM-ID in SESSION.
DIRECTION must be \"b\" (the default) or \"f\".  LIMIT is the
maximum number of events to return (default 10)."
  ;; TODO: As written, this may only work going backward.  Needs testing.
  (with-slots (id session prev-batch last-full-sync) room
    (matrix-get session (format "rooms/%s/messages" id)
                (a-list 'from prev-batch
                        'to last-full-sync
                        'dir direction
                        'limit limit)
                (apply-partially #'matrix-messages-callback room))))

(matrix-defcallback messages matrix-room
  "Callback for /rooms/{roomID}/messages."
  :slots (id timeline timeline-new prev-batch last-full-sync)
  :body (pcase-let* (((map start end chunk) data))
          ;; NOTE: API docs:
          ;; start: The token the pagination starts from. If dir=b
          ;; this will be the token supplied in from.
          ;; end: The token the pagination ends at. If dir=b this
          ;; token should be used again to request even earlier
          ;; events.
          (seq-doseq (event chunk)
            (push event timeline)
            (push event timeline-new))

          (if (equal end last-full-sync)
              ;; Gap has been filled: clear the last-full-sync token (NOTE: Not sure if this is correct)
              (progn
                (matrix-log "MESSAGES CALLBACK for ROOM: %s: gap is filled: %s" id data)
                (setq last-full-sync nil))
            ;; Gap not yet filled: continue filling
            (matrix-log "MESSAGES CALLBACK for ROOM: %s: gap NOT filled: %s" id data)
            (setq prev-batch end)
            (matrix-messages room))))

(cl-defmethod matrix-sync-ephemeral ((room matrix-room) ephemeral)
  "Sync EPHEMERAL in ROOM."
  (with-slots (ephemeral) room
    (pcase-let (((map events) ephemeral))
      (seq-doseq (event events)
        (push event ephemeral)))))

(cl-defmethod matrix-sync-account_data ((session matrix-session) data)
  "Sync ACCOUNT-DATA in SESSION."
  (with-slots (account-data) session
    (pcase-let (((map events) data))
      (seq-doseq (event events)
        (push event account-data)))))

(cl-defmethod matrix-sync-account_data ((room matrix-room) data)
  "Sync ACCOUNT-DATA in ROOM."
  (with-slots (account-data) room
    (pcase-let (((map events) data))
      (seq-doseq (event events)
        (push event account-data)))))

(cl-defmethod matrix-sync-to_device ((session matrix-session) data)
  "Sync to_device data in SESSION."
  ;; FIXME: Implement.
  (matrix-log "Received to_device data: %s" data))

(cl-defmethod matrix-sync-device_lists ((session matrix-session) data)
  "Sync device_lists data in SESSION."
  ;; FIXME: Implement.
  (matrix-log "Received device_lists data: %s" data))

(cl-defmethod matrix-sync-unread_notifications ((room matrix-room) unread-notifications)
  "Sync UNREAD-NOTIFICATIONS in ROOM."
  (pcase-let (((map highlight_count notification_count) unread-notifications))
    (matrix-log "Would process highlight_count in %s: " room highlight_count)
    (matrix-log "Would process notification_count in %s: " room notification_count)
    t))

;;;;; Rooms

(cl-defmethod matrix-create-room ((session matrix-session)
                                  &key (visibility "private") alias name topic invite preset (is-direct t))
  "Create new room on SESSION.
When IS-DIRECT is non-nil, set that flag on the new room."
  ;; https://matrix.org/docs/spec/client_server/r0.3.0.html#id190

  ;; MAYBE: Add other parameters: invite_3pid, creation_content,
  ;; initial_state.  Not sure how useful these would be for us.

  (matrix-post session 'createRoom (a-list 'visibility visibility
                                           'room_alias_name alias
                                           'name name
                                           'topic topic
                                           'preset preset
                                           'is-direct is-direct)
               #'matrix-create-room-callback))

(matrix-defcallback create-room matrix-session
  "Callback for create-room.
Add new room to SESSION."
  ;; TODO: Should we add the room directly here, or should we do that after /sync?
  :slots (rooms)
  :body (pcase-let* (((map room_id) data)
                     (room (matrix-room :session session
                                        :id room_id)))
          (push room rooms)))

(cl-defmethod matrix-send-message ((room matrix-room) message &key (msgtype "m.text"))
  "Send MESSAGE of MSGTYPE to ROOM."
  ;; https://matrix.org/docs/spec/client_server/r0.3.0.html#id182
  (with-slots* (((id session) room)
                ((txn-id) session))
    ;; Use `with-slots*' instead of `pcase-let*' so we can `incf' the txn-id.
    (let* ((type "m.room.message")
           (content (a-list 'msgtype msgtype
                            'body message))
           (txn-id (cl-incf txn-id))
           (endpoint (format "rooms/%s/send/%s/%s"
                             id type txn-id)))
      (matrix-put session endpoint content
                  (apply-partially #'matrix-send-message-callback room)))))

(matrix-defcallback send-message matrix-room
  "Callback for send-message."
  ;; For now, just log it, because we'll get it back when we sync anyway.
  :slots (id)
  :body (matrix-log "Message \"%s\" sent to room %s. Event ID: %s"
                    id message (a-get data 'event_id)))

(cl-defmethod matrix-leave ((room matrix-room))
  "Leave room."
  ;; https://matrix.org/docs/spec/client_server/r0.3.0.html#id203
  (with-slots (id session) room
    (let* ((endpoint (format "rooms/%s/leave" id)))
      (matrix-post session endpoint nil
                   (apply-partially #'matrix-leave-callback room)))))

(matrix-defcallback leave matrix-room
  "Leave room callback."
  :slots (session)
  ;; TODO: Verify that this works in more circumstances.  `equal' is
  ;; used, and it works in the test, but will it always work?
  :body (object-remove-from-list session :rooms room))

(cl-defmethod matrix-forget ((room matrix-room))
  "Forget ROOM."
  ;; TODO: Maybe use room ID instead of object, since if we've left a
  ;; room, the object should be gone.  Alternatively, instead of
  ;; removing the room when it's left, change its status, or put it in
  ;; a list of left-but-not-forgotten rooms.  Should look at how the
  ;; API does it more, probably in /sync with the different lists of
  ;; rooms.

  ;; https://matrix.org/docs/spec/client_server/r0.3.0.html#id204
  (with-slots (id session) room
    (let* ((endpoint (format "rooms/%s/forget" id)))
      (matrix-post session endpoint nil
                   (apply-partially #'matrix-forget-callback room)))))

(matrix-defcallback forget matrix-room
  "Forget room callback."
  :slots (id)
  :body (matrix-log "FORGOT ROOM: %s" id))

(cl-defmethod matrix-typing ((room matrix-room) (typing t))
  "Send TYPING notification to ROOM.
TYPING should be t or nil."
  (pcase-let* (((eieio id session) room)
               ((eieio user) session)
               (endpoint (format "rooms/%s/typing/%s" id user))
               (data (a-list 'typing typing
                             'timeout 30000)))
    (matrix-put session endpoint data #'ignore)))

;;;; Footer

(provide 'matrix-api-r0.3.0)

;;; matrix-api-r0.3.0.el ends here
