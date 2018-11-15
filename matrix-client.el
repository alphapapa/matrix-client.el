;;; matrix-client.el --- A matrix client ---  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Adam Poter
;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Jay Kamat <jaygkamat@gmail.com>
;; Created: 28 October 2018
;; Keywords: web, comm
;; Homepage: https://github.com/jgkamat/matrix-client-el
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (dash "2.13.0") (dash-functional "1.2.0") (f "0.17.2") (request "0.2.0") (a "0.1.0") (ov "1.0.6") (s "1.12.0") (tracking "2.9") (esxml "0.3.4") (ht "2.2"))

;; This file is not part of GNU Emacs.

;; matrix-client.el is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; matrix-client.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Commentary.

;;; Code:

;; ARGH EMACS 26 WHY
(unless (fboundp 'if-let)
  (defalias 'if-let 'if-let*)
  (defalias 'when-let 'when-let*))

;;;; Requirements

(require 'cl-lib)
(require 'calendar)

(require 'f)
(require 'ov)
(require 'tracking)

(require 'matrix-api-r0.3.0)
(require 'matrix-client-faces)
(require 'matrix-client-room)
(require 'matrix-notifications)
(require 'matrix-client-images)

;;;; Variables

(defvar matrix-client-sessions nil
  "List of active sessions.")

(defvar matrix-client-mark-modified-rooms t)

(defvar matrix-client-input-prompt "▶ ")

(defvar matrix-client-midnight-timer nil
  "Timer used to update date headers at midnight.")

(defcustom matrix-client-render-presence t
  "Show presence changes in the main buffer windows."
  :type 'boolean)

(defcustom matrix-client-render-membership t
  "Show membership changes in the main buffer windows."
  :type 'boolean)

(defcustom matrix-client-render-html (featurep 'shr)
  "Render HTML messages in buffers. These are currently the
ad-hoc 'org.matrix.custom.html' messages that Vector emits."
  :type 'boolean)

(defcustom matrix-client-save-token nil
  "Save username and access token upon successful login."
  :type 'boolean)

(defcustom matrix-client-save-token-file "~/.cache/matrix-client.el.token"
  "Save username and access token to this file."
  :type 'file)

(defcustom matrix-client-use-tracking nil
  "Enable tracking.el support in matrix-client."
  :type 'boolean)

(defcustom matrix-client-save-outgoing-messages t
  "Save outgoing messages in kill ring before sending.
This way, in the event that a message gets lost in transit, the
user can recover it from the kill ring instead of retyping it."
  :type 'boolean)

;;;; Classes

(matrix-defclass matrix-room-client-data ()
  ((buffer :initarg :buffer)
   (notification-rules :initarg :notification-rules
                       :documentation "List of functions to test events against; if one returns non-nil, a notification should be displayed."))
  "Client data data stored in room objects.")

;;;; Mode

(define-derived-mode matrix-client-mode fundamental-mode "Matrix"
  "Mode for Matrix room buffers."
  :group 'matrix-client
  ;; TODO: Add a new abbrev table that uses usernames, rooms, etc.
  :keymap matrix-client-mode-map)

;;;; Connect / disconnect

;;;###autoload
(defun matrix-client-connect (&optional user password access-token server)
  "Matrix Client NG"
  (interactive)
  (if matrix-client-sessions
      ;; TODO: Already have active session: display list of buffers
      ;; FIXME: If login fails, it still shows as active.
      (message "Already active")
    ;; No existing session
    (if-let ((enabled matrix-client-save-token)
             (saved (matrix-client-load-token)))
        ;; Use saved token
        ;; FIXME: Change "username" to "user" when we no longer need compatibility with old code
        (setq user (a-get saved 'username)
              server (a-get saved 'server)
              access-token (a-get saved 'token)
              txn-id (a-get saved 'txn-id))
      ;; Not saved: prompt for username and password
      (setq user (or user (read-string "User ID: "))
            password (or password (read-passwd "Password: "))
            server (or server
                       (--> (read-passwd "Server (leave blank to derive from user ID): ")
                            (if (string-empty-p it)
                                nil
                              it)))))
    (if access-token
        ;; Use saved token and call post-login hook
        (matrix-client-login-hook (matrix-session :user user
                                                  :server server
                                                  :access-token access-token
                                                  :txn-id txn-id
                                                  :initial-sync-p t))
      ;; Log in with username and password
      (matrix-login (matrix-session :user user
                                    :server server
                                    :initial-sync-p t)
                    password))))

(cl-defmethod matrix-client-login-hook ((session matrix-session))
  "Callback for successful login.
Add session to sessions list and run initial sync."
  (push session matrix-client-sessions)
  (matrix-sync session)
  (when matrix-client-save-token
    (matrix-client-save-token session))
  ;; NOTE: What happens if the system is asleep at midnight?
  (setq matrix-client-midnight-timer (run-at-time "00:00" 86400 #'matrix-client-update-all-date-headers))
  (message "Jacked in to %s.  Syncing..." (oref session server)))

(add-hook 'matrix-login-hook #'matrix-client-login-hook)

(cl-defmethod matrix-client-rename-room-buffers ((session matrix-session))
  "Rename all room buffers in SESSION.
Should be called after initial sync."
  ;; After initial sync timelines are processed, we run the room metadata hook to set the
  ;; room buffer names (which we do not do during processing of timelines during initial
  ;; sync, because doing so for every user "join" event is very slow.
  (dolist (room (oref session rooms))
    (matrix-client-rename-buffer room)))

(add-hook 'matrix-after-initial-sync-hook #'matrix-client-rename-room-buffers)

(cl-defmethod matrix-client-save-token ((session matrix-session))
  "Save username and access token for session SESSION to file."
  ;; FIXME: This does not work with multiple sessions.
  ;; MAYBE: Could we use `savehist-additional-variables' instead of our own code for this?
  ;; TODO: Check if file exists; if so, ensure it has a proper header so we know it's ours.
  (with-temp-file matrix-client-save-token-file
    (with-slots (user server access-token txn-id) session
      ;; FIXME: Change "username" to "user" when we no longer need compatibility with old code
      ;; FIXME: Change token to access-token for clarity.
      (prin1 (a-list 'username user
                     'server server
                     'token access-token
                     'txn-id txn-id)
             (current-buffer))))
  ;; Ensure permissions are safe
  (chmod matrix-client-save-token-file #o600))

(defun matrix-client-load-token ()
  "Return saved username and access token from file."
  (when (f-exists? matrix-client-save-token-file)
    (read (f-read matrix-client-save-token-file))))

(defun matrix-client-disconnect (&optional logout)
  "Unplug from the Matrix.
If LOGOUT is non-nil, actually log out, canceling access
tokens (username and password will be required again)."
  (interactive "P")
  (cond (logout (seq-do #'matrix-logout matrix-client-sessions)
                ;; Remove saved token
                (f-delete matrix-client-save-token-file))
        ;; FIXME: This does not work for multiple sessions.
        (t (matrix-client-save-token (car matrix-client-sessions))))
  (--each matrix-client-sessions
    ;; Kill pending sync response buffer processes
    (with-slots (pending-syncs disconnect) it
      (setq disconnect t)
      (ignore-errors
        ;; Ignore errors in case of "Attempt to get process for a dead buffer"
        (seq-do #'delete-process pending-syncs)))
    ;; Kill buffers
    (with-slots (rooms) it
      (--each rooms
        (kill-buffer (oref* it client-data buffer))))
    ;; Try to GC the session object.  Hopefully no timers or processes or buffers still hold a ref...
    (setf it nil))
  (cancel-timer matrix-client-midnight-timer)
  (setq matrix-client-midnight-timer nil)
  (setq matrix-client-sessions nil))

;;;; Commands

(defun matrix-client-switch-buffer ()
  "Prompt for a Matrix buffer and switch to it."
  (interactive)
  (--when-let (completing-read "Room: " (->> (oref* (car matrix-client-sessions) rooms)
                                             (--map (buffer-name (oref* it client-data buffer)))
                                             (-sort #'string-collate-lessp)))
    (switch-to-buffer it)))

;;;; Rooms

(defun matrix-client-update-all-date-headers ()
  "Update date headers in all rooms.
Intended to be called from a timer that runs at midnight."
  (dolist (session matrix-client-sessions)
    (dolist (room (oref session rooms))
      (with-room-buffer room
        (matrix-client--update-date-headers)))))

;;;;; Timeline

(cl-defmethod matrix-client-timeline ((room matrix-room) event)
  "Process EVENT in ROOM."
  (pcase-let* (((map type) event))
    (apply-if-fn (concat "matrix-client-event-" type)
        (list room event)
      (matrix-unimplemented (format$ "Unimplemented client method: $fn-name")))))

;;;; Helper functions

(defun matrix-client-event-timestamp (data)
  "Return timestamp of event DATA."
  (let ((server-ts (float (a-get* data 'origin_server_ts)))
        (event-age (float (or (a-get* data 'unsigned 'age)
                              0))))
    ;; The timestamp and the age are in milliseconds.  We need
    ;; millisecond precision in case of two messages sent/received
    ;; within one second, but we need to return seconds, not
    ;; milliseconds.  So we divide by 1000 to get the timestamp in
    ;; seconds, but we keep millisecond resolution by using floats.
    (/ (- server-ts event-age) 1000)))

(defun matrix-client-linkify-urls (text)
  "Return TEXT with URLs in it made clickable."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (cl-loop while (re-search-forward (rx bow "http" (optional "s") "://" (1+ (not space))) nil 'noerror)
             do (make-text-button (match-beginning 0) (match-end 0)
                                  'mouse-face 'highlight
                                  'face 'link
                                  'help-echo (match-string 0)
                                  'action #'browse-url-at-mouse
                                  'follow-link t))
    (buffer-string)))

(defun matrix-client-buffer-visible-p (&optional buffer)
  "Return non-nil if BUFFER is currently visible.
If BUFFER is nil, use the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (or (eq buffer (window-buffer (selected-window)))
        (get-buffer-window buffer))))

(defun matrix--calendar-absolute-to-timestamp (absolute)
  "Convert ABSOLUTE day number to Unix timestamp.
Does not account for leap seconds.  ABSOLUTE should be the number
of days since 0000-12-31, e.g. as returned by
`calendar-absolute-from-gregorian'."
  ;; NOTE: This function should come with Emacs!
  (let* ((gregorian (calendar-gregorian-from-absolute absolute))
         (days-between (1+ (days-between (format "%s-%02d-%02d 00:00" (cl-caddr gregorian) (car gregorian) (cadr gregorian))
                                         "1970-01-01 00:00")))
         (seconds-between (* 86400 days-between)))
    (string-to-number (format-time-string "%s" seconds-between))))

(defun matrix-client--human-format-date (date)
  "Return human-formatted DATE.
DATE should be either an integer timestamp, or a string in
\"YYYY-MM-DD HH:MM:SS\" format.  The \"HH:MM:SS\" part is
optional."
  ;; NOTE: This seems to be the only format that `days-between' (and `date-to-time') accept.  This
  ;; appears to be undocumented; it just says, "date-time strings" without specifying what KIND of
  ;; date-time strings, leaving you to assume it pl.  I only found this out by trial-and-error.
  (setq date (cl-typecase date
               (integer (format-time-string "%F" (seconds-to-time date)))
               (float (format-time-string "%F" (seconds-to-time date)))
               (list (format-time-string "%F" (seconds-to-time date)))
               (string date)))
  (unless (string-match (rx (= 2 digit) ":" (= 2 digit) eos) date)
    (setq date (concat date " 00:00")))
  (let* ((difference (days-between (format-time-string "%F %T") date)))
    (cond ((= 0 difference) "Today")
          ((= 1 difference) "Yesterday")
          ((< difference 7) (format-time-string "%A" (date-to-time date)))
          (t (format-time-string "%A, %B %d, %Y" (date-to-time date))))))

;;;; Footer

(provide 'matrix-client)

;;; matrix-client.el ends here
