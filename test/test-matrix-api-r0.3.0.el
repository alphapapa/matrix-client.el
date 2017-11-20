(require 'cl-lib)
(require 'eieio)
(require 'map)
(require 'seq)

(require 'a)
(require 'buttercup)
(require 'f)
(require 'json)
(require 'request)


(describe "matrix-client"

  (let ((load-path (list default-directory
                         (f-parent default-directory)
                         (f-join default-directory "test"))))
    ;;  (require 'matrix-api-r0.3.0)
    (load "matrix-api-r0.3.0")
    (load "password"))

  (before-all
    (setq session (matrix-session :user "@matrix-client-el-tester:matrix.org"
                                  :device-id (md5 (concat "matrix-client.el/test.el" (current-time-string) (system-name)))
                                  :initial-device-display-name (format "matrix-client.el/test.el @ %s (%s)"
                                                                       (system-name) (format-time-string "%F %T"))))
    (setq matrix-synchronous t))

  (it "Can log in and get an access_token"
    (matrix-login session matrix-password)
    (expect (oref session access-token)
            :to-match (rx (1+ alnum))))

  ;; TODO: If we create a new room, we can send messages to it, then
  ;; test retrieving them with sync, and then leave/delete the room
  ;; afterward.

  ;; (it "Can create a room, join it, and send messages to it")

  ;; (it "Can do an initial sync"
  ;;   (matrix-sync session)
  ;;   ;; There should be some timeline event objects in the first room.
  ;;   (expect (length (oref (car (oref session rooms)) timeline))
  ;;           :to-be-greater-than 0))

  (it "Can create a room"
    (matrix-create-room session)
    (expect (length (oref session rooms))
            :to-be-greater-than 0))

  (it "Can send messages to a room"
    (matrix-send-message (car (oref session rooms)) "Test message."))

  (it "Can do a subsequent sync"
    (matrix-sync session)
    (expect (length (oref (car (oref session rooms)) timeline))
            :to-be-greater-than 0)
    (expect (a-get* (car (oref (car (oref session rooms)) timeline)) 'content 'body)
            :to-equal "Test message."))


  (xit "Can fetch more messages"
    (pcase-let* ((room (car (oref session rooms)))
                 ((eieio id) room))
      (matrix-messages session id))
    (expect (length (oref (car (oref session rooms)) timeline))
            ;; The default number to fetch at one time is 10, so
            ;; after syncing and fetching more, there should be more
            ;; than 10.
            ;; FIXME: That is, given that the room has more than 10
            ;; events in it.  If we start testing by making our own
            ;; room, we'll have to ensure we add more than 10
            ;; events.
            :to-be-greater-than 10)))
