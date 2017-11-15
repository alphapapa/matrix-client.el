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
             :success #'matrix-client-sync-callback)))

(cl-defun matrix-client-sync-callback (&key data error-thrown symbol-status response &allow-other-keys)
  "Process RESPONSE from `matrix-client-sync`."
  (message "%s" (-map #'matrix-client-sync-room-join
                      (a-get* data 'rooms 'join))))

(defun matrix-client-sync-room-join (room)
  "Sync join events in ROOM.
ROOM should be an alist of the \"join\" object, as returned by
the Matrix API, not a matrix-client-room object, in the
form (ROOM-ID (ACCOUNT_DATA...) (EPHEMERAL...) ...)."
  (pcase-let* ((`(,id . ,attrs) room)
               ((map ephemeral state timeline) attrs)
               ((map events limited prev_batch) timeline))
    (cl-loop for event across events
             collect (a-get* event 'content))))



(when nil
  ;; Easy way to stop these from being evaluated when the whole buffer
  ;; is.

  (--each matrix-client-connections
    (matrix-client-sync (cdr it))))
