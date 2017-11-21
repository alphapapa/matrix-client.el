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
    (setq matrix-synchronous t)
    (defvar matrix-test-joined-rooms nil))

  (it "Can log in and get an access_token"
    (matrix-login session matrix-password)
    (expect (oref session access-token)
            :to-match (rx (1+ alnum))))

  (describe "Rooms"

    (it "Begins with no rooms"
      (matrix-sync session)
      (expect (length (oref session rooms))
              :to-be 0))

    (it "Can create a room"
      (matrix-create-room session)
      ;; Used to forget the room later
      (push (car (oref session rooms)) matrix-test-joined-rooms)
      (expect (length (oref session rooms))
              :to-be-greater-than 0))

    (describe "Messages"

      (it "Can send messages to a room"
        (spy-on #'matrix-send-message-callback :and-call-through)
        (matrix-send-message (car (oref session rooms)) "Test message.")
        (expect #'matrix-send-message-callback :to-have-been-called))

      (it "Can sync messages from a room"
        ;; TODO: This is more generic and wide-ranging than just rooms
        ;; or messages.  Maybe we need to do two sessions, and sync
        ;; previous messages in the next one.
        (matrix-sync session)
        (expect (length (oref (car (oref session rooms)) timeline))
                :to-be-greater-than 0)
        (expect (pcase-let* (((eieio rooms) session)
                             (room (car rooms))
                             ((eieio timeline) room)
                             (first-event (car timeline)))
                  (a-get* first-event 'content 'body))
                :to-equal "Test message.")))

    (xit "Can change room name")

    (xit "Can change room topic")

    (xit "Can change room alias")

    (it "Can leave a room"
      (matrix-leave (car (oref session rooms)))
      (expect (length (oref session rooms))
              :to-equal 0))

    (it "Can forget a room"
      (matrix-forget (car matrix-test-joined-rooms)))

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

  (describe "Initial sync"

    ;; TODO: Need a second test account that remains in rooms with messages for testing initial sync.

)



  )
