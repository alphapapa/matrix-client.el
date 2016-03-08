(defun matrix-client-reconnect (arg)
  "Reconnect to Matrix.

Without a `prefix-arg' ARG it will simply restart the
matrix-client-stream poller, but with a prefix it will disconnect and
connect, clearing all room data."
  (interactive "P")
  (if (or arg (not matrix-client-event-stream-end-token))
      (progn
        (matrix-client-disconnect)
        (matrix-client))
    (matrix-client-stream-from-end-token)))

(defun matrix-client-disconnect ()
  "Disconnect from Matrix and kill all active room buffers."
  (interactive)
  (dolist (room-cons matrix-client-active-rooms)
    (kill-buffer (cdr room-cons)))
  (setq matrix-client-active-rooms nil)
  (setq matrix-client-event-listener-running nil))

(defun matrix-client-event-listener-callback (data)
  "The callback which `matrix-event-poll' pushes its data in to.

This calls each function in matrix-client-new-event-hook with the data
object with a single argument, DATA."
  (setq matrix-client-watchdog-last-message-ts
        (time-to-seconds))
  (unless (eq (car data) 'error)
    (dolist (hook matrix-client-new-event-hook)
      (funcall hook data)))
  (matrix-client-start-event-listener (matrix-get 'end data)))

(defun matrix-client-render-events-to-room (data)
  "Given a chunk of data from an /initialSyc, render each element from DATA in to its room."
  (let ((chunk (matrix-get 'chunk data)))
    (mapc 'matrix-client-render-event-to-room chunk)))

(defun matrix-client-filter (condp lst)
  "A standard filter, feed it a function CONDP and a LST."
  (delq nil
        (mapcar (lambda (x)
                  (and (funcall condp x) x))
                lst)))

(defun matrix-client-set-room-end-token (data)
  "When an event DATA comes in, file it in to the room so that we can mark a cursor when visiting the buffer."
  (mapc (lambda (data)
          (let* ((room-id (matrix-get 'room_id data))
                 (room-buf (matrix-get room-id matrix-client-active-rooms)))
            (when room-buf
              (with-current-buffer room-buf
                (setq matrix-client-room-end-token (matrix-get 'event_id data)))))
          ) (matrix-get 'chunk data)))

(defun matrix-client-restart-listener-maybe (sym error-thrown)
  "The error handler for matrix-client's event-poll.

SYM and ERROR-THROWN come from Request and are used to decide whether to connect."
  (cond ((or (string-match "code 6" (cdr error-thrown))
             (eq sym 'parse-error)
             (eq sym 'timeout)
             (string-match "interrupt" (cdr error-thrown))
             (string-match "code 7" (cdr error-thrown)))
         (message "Lost connection with matrix, will re-attempt in %s ms"
                  (/ matrix-client-event-poll-timeout 2))
         (matrix-client-restart-later))
        ((string-match "code 60" (cdr error-thrown))
         (message "curl couldn't validate CA, not advising --insecure? File bug pls."))))

(defun matrix-client-stream-from-end-token ()
  "Restart the matrix-client stream from the saved end-token."
  (matrix-client-start-event-listener matrix-client-event-stream-end-token))

(defun matrix-client-restart-later ()
  "Try to restart the Matrix poller later, maybe."
  (run-with-timer (/ matrix-client-event-poll-timeout 1000) nil
                  'matrix-client-stream-from-end-token))

