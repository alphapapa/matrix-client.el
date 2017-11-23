(require 'cl-lib)
(require 'eieio)
(require 'map)
(require 'seq)

(require 'a)
(require 'buttercup)
(require 'f)
(require 'ht)
(require 'json)
(require 'request)


(describe "matrix-client"

  ;; Load/update API files
  (let ((load-path (list default-directory
                         (f-parent default-directory)
                         (f-join default-directory "test"))))
    (if (featurep 'matrix-api-r0.3.0)
        ;; Already loaded, e.g. running `buttercup-run-at-point' in a
        ;; running Emacs instance: `load' the files to get changes.
        (load "matrix-api-r0.3.0")
      ;; Probably running from Cask: just `require'.
      (require 'matrix-api-r0.3.0))
    ;; Load test account
    (load "account"))

  (before-all
    (setq matrix-test-session (matrix-session :user matrix-test-user
                                              :device-id (md5 (concat "matrix-client.el/test.el" (current-time-string) (system-name)))
                                              :initial-device-display-name (format "matrix-client.el/test.el @ %s (%s)"
                                                                                   (system-name) (format-time-string "%F %T"))))
    (setq matrix-synchronous t)
    (defvar matrix-test-joined-rooms nil))

  (it "Can log in and get an access_token"
    (matrix-login matrix-test-session matrix-test-password)

    (expect (oref matrix-test-session access-token)
            :to-match (rx (1+ alnum))))

  (it "Can do initial sync"
    (spy-on #'matrix-sync-callback :and-call-through)
    (matrix-sync matrix-test-session)

    (expect #'matrix-sync-callback :to-have-been-called)
    (expect (oref matrix-test-session next-batch) :to-be-truthy))

  (describe "Rooms"

    (before-all
      (setq matrix-rooms-initial-length (length (oref matrix-test-session rooms))))

    (it "Can create a room"
      (matrix-create-room matrix-test-session)
      ;; Used to forget the room later
      (push (car (oref matrix-test-session rooms)) matrix-test-joined-rooms)

      (expect (length (oref matrix-test-session rooms))
              :to-be-greater-than matrix-rooms-initial-length))

    (describe "Messages"

      (it "Can send messages to a room"
        (spy-on #'matrix-send-message-callback :and-call-through)
        (matrix-send-message (car (oref matrix-test-session rooms)) "Test message.")

        (expect #'matrix-send-message-callback :to-have-been-called))

      (it "Can sync messages from a room"
        ;; TODO: This is more generic and wide-ranging than just rooms
        ;; or messages.  Maybe we need to do two sessions, and sync
        ;; previous messages in the next one.
        (matrix-sync matrix-test-session)

        (expect (length (oref (car (oref matrix-test-session rooms)) timeline))
                :to-be-greater-than 0)
        (expect (pcase-let* (((eieio rooms) matrix-test-session)
                             (room (car rooms))
                             ((eieio timeline) room)
                             (first-event (car timeline)))
                  (a-get* first-event 'content 'body))
                :to-equal "Test message.")))

    (xit "Can change room name")

    (xit "Can change room topic")

    (xit "Can change room alias")

    (it "Can leave a room"
      (matrix-leave (car (oref matrix-test-session rooms)))

      (expect (length (oref matrix-test-session rooms))
              :to-equal matrix-rooms-initial-length))

    (it "Can forget a room"
      (matrix-forget (car matrix-test-joined-rooms)))

    (xit "Can fetch more messages"
      (pcase-let* ((room (car (oref matrix-test-session rooms)))
                   ((eieio id) room))
        (matrix-messages matrix-test-session id))
      (expect (length (oref (car (oref matrix-test-session rooms)) timeline))
              ;; The default number to fetch at one time is 10, so
              ;; after syncing and fetching more, there should be more
              ;; than 10.
              ;; FIXME: That is, given that the room has more than 10
              ;; events in it.  If we start testing by making our own
              ;; room, we'll have to ensure we add more than 10
              ;; events.
              :to-be-greater-than 10)))

  (it "Can log out"
    (spy-on #'matrix-logout-callback :and-call-through)
    (matrix-logout matrix-test-session)

    (expect #'matrix-logout-callback :to-have-been-called)
    (with-slots (access-token device-id) matrix-test-session
      (expect access-token :to-be nil)
      (expect device-id :to-be nil))))
