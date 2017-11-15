;; Here is a playground for implementing the latest version of the
;; API, r0.2.0.  Confusingly, "v1" of the API is older and more
;; primitive than "r0".  Apparently the API was not considered
;; "released" then, and now it is, so the versions are named with an R
;; instead of a V.

(cl-defmethod matrix-client-sync ((room matrix-client-room))
  ;; https://matrix.org/docs/spec/client_server/r0.2.0.html#id126
  (let ((token (oref* room :con :token)))
    (request-response-data
     (request (concat matrix-homeserver-base-url "/_matrix/client/r0/sync")
              :params (a-list 'access_token token)
              :sync t
              :error (apply-partially #'matrix-request-error-handler con)
              :parser 'json-read))))

(pcase-let* ((`(,username . ,connection) (car matrix-client-connections))
             ((eieio rooms) connection)
             (`(,display-name . ,room) (car rooms))
             (result (matrix-client-sync room)))
  (when result
    (cl-loop for (id . attrs) in (a-get* result 'rooms 'join)
             collect (a-list 'id id
                             'events (cl-loop for event across (a-get* attrs 'timeline 'events)
                                              collect event)))))

(cl-defmethod matrix-client-sync ((con matrix-client-connection))
  (with-slots (token) con
    (let ((response (request-response-data
                     (request (concat matrix-homeserver-base-url "/_matrix/client/r0/sync")
                              :params (a-list 'access_token token)
                              :sync t
                              :error (apply-partially #'matrix-request-error-handler con)
                              :parser 'json-read))))
      (-map #'matrix-client-sync-room-join (a-get* response 'rooms 'join))
      ;;  (a-get* response 'rooms 'join)
      ;; response
      )))

(defun matrix-client-sync-room-join (room)
  "Sync events in ROOM.
ROOM should be an alist as returned by the Matrix API, not a
matrix-client-room object."
  (pcase-let* ((`(,id . ,attrs) room)
               ((map ephemeral state timeline) attrs)
               ((map events limited prev_batch) timeline))
    (cl-loop for event across events
             collect (a-get* event 'content))))

(matrix-client-sync (cdar matrix-client-connections))

(--map (matrix-client-sync (cdr it))
       matrix-client-connections)

(pcase-let* ((`(,username . ,connection) (car matrix-client-connections))
             ((eieio rooms) connection)
             (`(,display-name . ,room) (car rooms))
             (result (matrix-client-sync room)))
  (when result
    (cl-loop for (id . attrs) in (a-get* result 'rooms 'join)
             collect (a-list 'id id
                             'events (cl-loop for event across (a-get* attrs 'timeline 'events)
                                              collect event)))))
(pcase-let* ((`(,username . ,connection) (car matrix-client-connections)))
  (with-slots (token) connection
    token))
