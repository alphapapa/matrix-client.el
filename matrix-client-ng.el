;;; matrix-client-ng.el --- summary ---  -*- lexical-binding: t; -*-

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
(require 'matrix-notifications)
(require 'matrix-client-images)

;;;; TEMP

(cl-defun matrix-client-notify-m.room.message (event &key room &allow-other-keys)
  "Show notification for m.room.message events.
EVENT should be the `event' variable from the
`defmatrix-client-handler'.  ROOM should be the room object."
  (pcase-let* (((map content sender event_id) event)
               ((map body) content)
               ((eieio extra) room)
               ((eieio buffer) extra)
               (display-name (matrix-user-displayname room sender))
               (id (notifications-notify :title (format$ "<b>$display-name</b>")
                                         ;; Encode the message as ASCII because dbus-notify
                                         ;; can't handle some Unicode chars.  And
                                         ;; `ignore-errors' doesn't work to ignore errors
                                         ;; from it.  Don't ask me why.
                                         :body (encode-coding-string body 'us-ascii)
                                         :category "im.received"
                                         :timeout 5000
                                         :app-icon nil
                                         :actions '("default" "Show")
                                         :on-action #'matrix-client-notification-show)))
    (map-put matrix-client-notifications id (a-list 'buffer buffer
                                                    'event_id event_id))
    ;; Trim the list
    (setq matrix-client-notifications (-take 20 matrix-client-notifications))))

;;;; Variables

(defvar matrix-client-ng-sessions nil
  "List of active sessions.")

(defvar matrix-client-ng-show-images
  ;; FIXME: Copy defcustom.
  nil)

(defvar matrix-client-ng-mark-modified-rooms t)

(defvar matrix-client-ng-input-prompt "▶ ")

(defcustom matrix-client-ng-render-presence t
  "Show presence changes in the main buffer windows."
  :type 'boolean)

(defcustom matrix-client-ng-render-membership t
  "Show membership changes in the main buffer windows."
  :type 'boolean)

(defcustom matrix-client-ng-render-html (featurep 'shr)
  "Render HTML messages in buffers. These are currently the
ad-hoc 'org.matrix.custom.html' messages that Vector emits."
  :type 'boolean)

(defcustom matrix-client-ng-save-token nil
  "Save username and access token upon successful login."
  :type 'boolean)

(defcustom matrix-client-ng-save-token-file "~/.cache/matrix-client.el.token"
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

;;;;; Faces

(defface matrix-client-metadata
  '((((class color) (background light)) (:foreground "#000088" :weight bold))
    (((class color) (background dark)) (:foreground "#4444FF" :weight bold))
    (t (:weight bold)))
  "Face for chat metadata properties."
  :group 'matrix-client)

(defface matrix-client-own-metadata
  '((((class color) (background light)) (:foreground "#268bd2" :weight bold))
    (((class color) (background dark)) (:foreground "#268bd2" :weight bold))
    (t (:weight bold)))
  "Face for user's own chat metadata properties."
  :group 'matrix-client)

(defface matrix-client-own-messages
  '((((class color) (background light)) (:foreground "#586e75" :weight bold :slant italic))
    (((class color) (background dark)) (:foreground "#586e75" :weight bold :slant italic))
    (t (:weight bold :slant italic)))
  "Face for user's own chat messages."
  :group 'matrix-client)

(defface matrix-client-notice
  '((t :inherit font-lock-comment-face))
  "Face for notices."
  :group 'matrix-client)

(defface matrix-client-notice-metadata
  '((t :inherit font-lock-comment-face))
  "Face for notices."
  :group 'matrix-client)

(defface matrix-client-last-seen
  '((t (:inherit 'highlight :height 0.1)))
  "Face for last-seen overlay."
  :group 'matrix-client)

(defface matrix-client-date-header
  '((t (:inherit highlight :weight bold)))
  "Face for date headers."
  :group 'matrix-client)

;;;; Macros

(defmacro with-room-buffer (room &rest body)
  (declare (debug (sexp body)) (indent defun))
  `(with-slots* (((extra id) room)
                 ((buffer) extra))
     (unless buffer
       ;; Make buffer if necessary.  This seems like the easiest way
       ;; to guarantee that the room has a buffer, since it seems
       ;; unclear what the first received event type for a joined room
       ;; will be.
       (setq buffer (get-buffer-create (matrix-client-ng-display-name room)))
       (matrix-client-ng-setup-room-buffer room))
     (with-current-buffer buffer
       ,@body)))

(cl-defmacro matrix-client-ng-defevent (type docstring &key object-slots event-keys content-keys let body)
  "Define a method on `matrix-room' to handle Matrix events of TYPE.

TYPE should be a symbol representing the event type,
e.g. `m.room.message'.

DOCSTRING should be a docstring for the method.

OBJECT-SLOTS should be a list of lists, each in the form (OBJECT
SLOT ...), which will be turned into a `with-slots*' form
surrounding the following `pcase-let*' and BODY.  (This form
seems more natural than the (SLOTS OBJECT) form used by
`with-slots'.)

The following are bound in order in `pcase-let*':

EVENT-KEYS should be a list of symbols in the EVENT alist which
are bound with `pcase-let*' around the body.  These keys are
automatically bound: `content', `event_id', `sender',
`origin_server_ts', `type', and `unsigned'.

CONTENT-KEYS should be a list of symbols in the EVENTs `content'
key, which are bound in the `pcase-let*' around the body.

LET should be a varlist which is bound in the `pcase-let*' around
the body.

BODY will finally be evaluated in the context of these slots and
variables.

It is hoped that using this macro is easier than defining a large
method without it."
  ;; FIXME: It would probably be better to use the same form for OBJECT-SLOTS that is used by
  ;; `pcase-let*', because having two different ways is too confusing.
  (declare (indent defun))
  (let ((method-name (intern (concat "matrix-client-event-" (symbol-name type))))
        (slots (cl-loop for (object . slots) in object-slots
                        collect (list slots object))))
    `(cl-defmethod ,method-name ((room matrix-room) event)
       ,docstring
       (with-slots* ,slots
         (pcase-let* (((map content event_id sender origin_server_ts type unsigned ,@event-keys) event)
                      ((map ,@content-keys) content)
                      ,@let)
           ,body)))))

;;;; Classes

(matrix-defclass matrix-room-extra ()
  ((buffer :initarg :buffer))
  "Extra data stored in room objects.")

;;;; Mode

(define-derived-mode matrix-client-ng-mode fundamental-mode "Matrix"
  "Mode for Matrix room buffers."
  :group 'matrix-client-ng
  ;; TODO: Add a new abbrev table that uses usernames, rooms, etc.
  :keymap matrix-client-ng-mode-map)

;;;;; Keys

(define-key matrix-client-ng-mode-map (kbd "RET") 'matrix-client-ng-send-input)
(define-key matrix-client-ng-mode-map (kbd "DEL") 'matrix-client-ng-delete-backward-char)

;;;; Connect / disconnect

;;;###autoload
(defun matrix-client-ng-connect (&optional user password access-token)
  "Matrix Client NG"
  (interactive)
  (if matrix-client-ng-sessions
      ;; TODO: Already have active session: display list of buffers
      ;; FIXME: If login fails, it still shows as active.
      (message "Already active")
    ;; No existing session
    (if-let ((enabled matrix-client-ng-save-token)
             (saved (matrix-client-ng-load-token)))
        ;; Use saved token
        ;; FIXME: Change "username" to "user" when we no longer need compatibility with old code
        (setq user (a-get saved 'username)
              access-token (a-get saved 'token)
              txn-id (a-get saved 'txn-id))
      ;; Not saved: prompt for username and password
      (setq user (or user (read-string "User ID: "))
            password (or password (read-passwd "Password: "))))
    (if access-token
        ;; Use saved token and call post-login hook
        (matrix-client-ng-login-hook (matrix-session :user user
                                                     :access-token access-token
                                                     :txn-id txn-id
                                                     :initial-sync-p t))
      ;; Log in with username and password
      (matrix-login (matrix-session :user user
                                    :initial-sync-p t)
                    password))))

(cl-defmethod matrix-client-ng-login-hook ((session matrix-session))
  "Callback for successful login.
Add session to sessions list and run initial sync."
  (push session matrix-client-ng-sessions)
  (matrix-sync session)
  (when matrix-client-ng-save-token
    (matrix-client-ng-save-token session))
  (message "Jacked in to %s.  Syncing..." (oref session server)))

(add-hook 'matrix-login-hook #'matrix-client-ng-login-hook)

(cl-defmethod matrix-client-ng-save-token ((session matrix-session))
  "Save username and access token for session SESSION to file."
  ;; TODO: Check if file exists; if so, ensure it has a proper header so we know it's ours.
  (with-temp-file matrix-client-ng-save-token-file
    (with-slots (user access-token txn-id) session
      ;; FIXME: Change "username" to "user" when we no longer need compatibility with old code
      ;; FIXME: Change token to access-token for clarity.
      (prin1 (a-list 'username user
                     'token access-token
                     'txn-id txn-id)
             (current-buffer))))
  ;; Ensure permissions are safe
  (chmod matrix-client-ng-save-token-file #o600))

(defun matrix-client-ng-load-token ()
  "Return saved username and access token from file."
  (when (f-exists? matrix-client-ng-save-token-file)
    (read (f-read matrix-client-ng-save-token-file))))

(defun matrix-client-ng-disconnect (&optional logout)
  "Unplug from the Matrix.
If LOGOUT is non-nil, actually log out, canceling access
tokens (username and password will be required again)."
  (interactive "P")
  (when logout
    (seq-do #'matrix-logout matrix-client-ng-sessions)
    ;; Remove saved token
    (f-delete matrix-client-ng-save-token-file))
  (--each matrix-client-ng-sessions
    ;; Kill pending sync response buffer processes
    (with-slots (pending-syncs disconnect) it
      (setq disconnect t)
      (ignore-errors
        ;; Ignore errors in case of "Attempt to get process for a dead buffer"
        (seq-do #'delete-process pending-syncs)))
    ;; Kill buffers
    (with-slots (rooms) it
      (--each rooms
        (kill-buffer (oref* it extra buffer))))
    ;; Try to GC the session object.  Hopefully no timers or processes or buffers still hold a ref...
    (setf it nil))
  (setq matrix-client-ng-sessions nil))

;;;; Rooms

(cl-defmethod matrix-client-ng-update-last-seen ((room matrix-room) &rest _)
  "Move the last-seen overlay to after the last message in ROOM."
  (with-room-buffer room
    ;; FIXME: Does this need to be when-let?  Shouldn't these always be found?
    (when-let ((seen-ov (car (ov-in 'matrix-client-last-seen)))
               (target-pos (1- (matrix-client--prompt-position))))
      (ov-move seen-ov target-pos target-pos))))

(cl-defmethod matrix-client-ng-insert ((room matrix-room) string)
  "Insert STRING into ROOM's buffer.
STRING should have a `timestamp' text-property."
  (with-room-buffer room
    (save-excursion
      (let* ((inhibit-read-only t) ; MAYBE: use buffer-read-only mode instead
             (timestamp (get-text-property 0 'timestamp string))
             (day-number (time-to-days timestamp))
             (event-id (get-text-property 0 'event_id string))
             (insertion-pos (matrix-client-ng--insertion-pos timestamp))
             (non-face-properties (cl-loop for (key val) on (text-properties-at 0 string) by #'cddr
                                           unless (eq key 'face)
                                           append (list key val))))
        (goto-char insertion-pos)
        ;; Ensure event before point doesn't have the same ID
        (unless (when-let ((previous-event-pos (matrix--prev-property-change (point) 'timestamp)))
                  (string-equal event-id (get-text-property previous-event-pos 'event_id)))
          ;; Insert the message
          (insert (apply #'propertize (concat string "\n") 'read-only t non-face-properties))
          ;; Update tracking
          (unless (matrix-client-buffer-visible-p)
            (set-buffer-modified-p t)
            (when matrix-client-use-tracking
              ;; TODO handle faces when receving highlights
              (tracking-add-buffer (current-buffer)))))))))

(defun matrix-client-ng--insertion-pos (timestamp)
  "Return insertion position for TIMESTAMP.
Creates a new header if necessary."
  (let* ((header-pos (matrix-client--get-date-header timestamp))
         (limit (or (matrix--next-property-change header-pos 'matrix-header-day-number)
                    (1- (matrix-client--prompt-position))))
         (next-timestamp-pos (matrix--next-property-change header-pos 'timestamp limit)))
    (catch 'found
      (while next-timestamp-pos
        (when (> (get-text-property next-timestamp-pos 'timestamp) timestamp)
          ;; Found greater timestamp: return its position
          (throw 'found next-timestamp-pos))
        ;; Look for next timestamp
        (setq next-timestamp-pos (matrix--next-property-change next-timestamp-pos 'timestamp limit)))
      ;; No more timestamps: return limit
      limit)))

(defun matrix-client--get-date-header (timestamp)
  "Return position of appropriate date header in current buffer for TIMESTAMP.
Creates a new header if necessary."
  (cl-labels ((prev-header-pos () (matrix--prev-property-change (point) 'matrix-header-day-number))
              (current-header-day-number () (get-text-property (point) 'matrix-header-day-number)))
    (let* ((target-day-number (time-to-days timestamp))
           (prompt (1- (matrix-client--prompt-position))))
      (goto-char prompt)
      (catch 'found
        (while t
          (if-let ((prev-header-pos (prev-header-pos)))
              (progn
                ;; Found a previous header
                (goto-char prev-header-pos)
                (let ((current-header-day-number (current-header-day-number)))
                  (cond ((= current-header-day-number target-day-number)
                         ;; Found correct header
                         (throw 'found prev-header-pos))
                        ((< current-header-day-number target-day-number)
                         ;; Found earlier header: insert new one after current header's position
                         (goto-char (or (matrix--next-property-change (point) 'matrix-header-day-number prompt)
                                        prompt))
                         (matrix-client-room--insert-date-header timestamp)
                         ;; Return position after new header
                         (throw 'found (point)))))
                ;; Wrong header: keep looking
                )
            ;; No more headers found: update other headers and insert new header here (this will
            ;; happen when the current date changes and a new message arrives, as well as when a
            ;; message arrives for the current date and is the first message in that room for the
            ;; date).  FIXME: Maybe we could be smarter about whether to update the other date
            ;; headers, to avoid doing it when unnecessary.
            (matrix-client--update-date-headers)
            (matrix-client-room--insert-date-header timestamp)
            ;; Return one character before the end of the new header.  This is sort of a tiny hack
            ;; that is simpler than handling the logic in `matrix-client-insert'.  It fixes the case
            ;; when a new header for older messages is first inserted.
            (throw 'found (1- (point)))))))))

(defun matrix-client-room--insert-date-header (timestamp)
  "Insert date header for TIMESTAMP at current position in current buffer."
  (let* ((visible-header (propertize (concat " " (matrix-client--human-format-date timestamp) "\n")
                                     'face 'matrix-client-date-header))
         (whole-header (propertize (concat "\n"
                                           visible-header
                                           "\n")
                                   'matrix-client-date-header t
                                   'matrix-header-day-number (time-to-days timestamp)
                                   'read-only t)))
    (insert whole-header)))

(cl-defmethod matrix-client-ng-display-name ((room matrix-room))
  "Return display name for ROOM."
  ;; https://matrix.org/docs/spec/client_server/r0.3.0.html#id267

  ;; FIXME: Make it easier to name the room separately from the room's buffer.  e.g. I want the
  ;; header line to have the official room name, but I want the buffer name in 1-on-1 chats to be
  ;; the other person's name.
  (cl-labels ((displaynames-sorted-by-id (members)
                                         (--> members
                                              (cl-sort it #'string< :key #'car)
                                              (--map (matrix-user-displayname room (car it))
                                                     it))))
    (with-slots (id name aliases members) room
      (pcase-let* (((eieio session) room)
                   ((eieio (user self)) session)
                   (members (cl-remove self members :test #'string= :key #'car)))
        (pcase (when members
                 (length members))
          (1 (matrix-user-displayname room (caar members)))
          (2 (s-join ", " (displaynames-sorted-by-id members)))
          ((or `nil (pred (< 0))) ;; More than 2
           (or name
               ;; FIXME: The API docs say to use the canonical_alias instead of aliases.
               (car aliases)
               (format "%s and %s others"
                       (car (displaynames-sorted-by-id members))
                       (1- (length members)))))
          (_
           ;; FIXME: The API says to use names of previous room
           ;; members if nothing else works, but I don't feel like
           ;; coding that right now, so we'll just use the room ID.
           id))))))

(defun matrix-client-ng-send-input ()
  "Send current input to current room."
  (interactive)
  (goto-char (matrix-client--prompt-position))
  (pcase-let* ((room matrix-client-ng-room)
               ((eieio session) room)
               (input (prog1
                          (buffer-substring-no-properties (point) (point-max))
                        (delete-region (point) (point-max))))
               ;; NOTE: A happy accident, `s-split-words' chops off the leading "/".
               (first-word (car (s-split-words input))))
    (unless (s-blank-str? input)
      (when matrix-client-save-outgoing-messages
        (push input kill-ring))
      (apply-if-fn (concat "matrix-client-ng-room-command-" first-word)
          ;; Special command
          (list room input)
        (progn
          ;; Normal message
          (matrix-send-message room input)
          (matrix-client-ng-update-last-seen room))))))

(cl-defmethod matrix-client-ng-room-command-join ((room matrix-room) input)
  "Join room on session.
INPUT should be, e.g. \"/join #room:matrix.org\"."
  (pcase-let* (((eieio session) room)
               (words (s-split (rx (1+ space)) input))
               (room-id (cadr words)))
    ;; Only accept one room
    (if (> (length words) 2)
        (user-error "Invalid /join command")
      (matrix-join-room session room-id))))

(cl-defmethod matrix-client-ng-room-command-me ((room matrix-room) input)
  "Send emote INPUT to ROOM.
INPUT should begin with \"/me\"."
  (let ((emote (s-join " " (cdr (s-split-words input)))))
    (matrix-send-message room emote :msgtype "m.emote")))

(cl-defmethod matrix-client-ng-room-command-who ((room matrix-room) input)
  "Print list of users to ROOM."
  (with-slots (members) room
    (matrix-client-ng-insert room (propertize (concat "Room members: "
                                                      (--> members
                                                           (--map (a-get (cdr it) 'displayname) it)
                                                           (--sort (string-collate-lessp it other nil 'ignore-case)
                                                                   it)
                                                           (s-join ", " it)))
                                              'timestamp (time-to-seconds)
                                              'face 'matrix-client-notice))
    (matrix-client-ng-update-last-seen room)))

(defun matrix-client-ng-delete-backward-char (n &optional kill-flag)
  "Delete backward unless the point is at the prompt or other read-only text."
  (interactive "p\nP")
  (unless (get-text-property (- (point) 2) 'read-only)
    (call-interactively #'delete-backward-char n kill-flag)))

;;;;; Setup

(cl-defmethod matrix-client-ng-setup-room-buffer ((room matrix-room))
  "Prepare and switch to buffer for ROOM-ID, and return room object."
  (with-room-buffer room
    (matrix-client-ng-mode)
    (visual-line-mode 1)
    (setq buffer-undo-list t)
    ;; Unset buffer's modified status when it's selected
    ;; FIXME: Reactivate this.
    ;; (when matrix-client-ng-mark-modified-rooms
    ;;   (add-hook 'buffer-list-update-hook #'matrix-client-ng-buffer-list-update-hook 'append 'local))
    (erase-buffer)
    (switch-to-buffer (current-buffer))
    ;; FIXME: Remove these or update them.
    ;; (set (make-local-variable 'matrix-client-room-connection) con)
    (setq-local matrix-client-ng-room room)
    (when matrix-client-use-tracking
      (tracking-mode 1)))
  (matrix-client-ng-insert-prompt room)
  (matrix-client-ng-insert-last-seen room))

(cl-defmethod matrix-client-ng-insert-last-seen ((room matrix-room))
  "Insert last-seen overlay into ROOM's buffer."
  (with-room-buffer room
    (when-let ((prompt-ov (car (ov-in 'matrix-client-prompt)))
               (target-pos (1- (ov-beg prompt-ov))))
      (ov target-pos target-pos
          'before-string (concat "\n" (propertize "\n\n" 'face 'matrix-client-last-seen))
          'matrix-client-last-seen t))))

(cl-defmethod matrix-client-ng-insert-prompt ((room matrix-room))
  "Insert prompt into ROOM's buffer."
  (with-room-buffer room
    (let ((inhibit-read-only t)
          (ov-sticky-front t))
      (goto-char (point-max))
      (insert (propertize "\n" 'read-only t)
              "\n")
      (ov (point) (point)
          'before-string (concat (propertize "\n"
                                             'face '(:height 0.1))
                                 matrix-client-ng-input-prompt)
          'matrix-client-prompt t))))

;;;;; Metadata

(cl-defmethod matrix-client-ng-metadata-updated ((room matrix-room))
  "Update metadata for ROOM."
  (matrix-client-ng-update-header room))

(add-hook 'matrix-room-metadata-hook #'matrix-client-ng-metadata-updated)

(cl-defmethod matrix-client-ng-update-header ((room matrix-room))
  "Update the header line of the current buffer for ROOM.
Also update prompt with typers."
  (unless (and (boundp 'tabbar-mode) tabbar-mode)
    ;; Disable when tabbar mode is on.  MAYBE: Remove this.
    (with-room-buffer room
      (pcase-let* (((eieio avatar typers name topic) room)
                   (name (when name
                           (propertize name 'face 'font-lock-keyword-face)))
                   (ov (car (ov-in 'matrix-client-prompt)))
                   (typers-string (s-join ", " (cl-loop for user across typers
                                                        collect (matrix-user-displayname room user))))
                   (prompt (if (> (length typers) 0)
                               (concat (propertize (concat "Typing: " typers-string)
                                                   'face 'font-lock-comment-face)
                                       "\n" matrix-client-ng-input-prompt)
                             matrix-client-ng-input-prompt)))
        (ov-set ov 'before-string prompt)
        (setq header-line-format (concat avatar
                                         ;; NOTE: Not sure if using `format' with an image-containing string works.
                                         (format$ "$name: $topic")))))))

;;;;; Timeline

(cl-defmethod matrix-client-ng-timeline ((room matrix-room) event)
  "Process EVENT in ROOM."
  (pcase-let* (((map type) event))
    (apply-if-fn (concat "matrix-client-event-" (symbol-name type))
        (list room event)
      (matrix-unimplemented (format$ "Unimplemented client method: $fn-name")))))

(matrix-client-ng-defevent m.room.message
  "Process m.room.message EVENT in ROOM."
  :object-slots ((room session)
                 (session user initial-sync-p))
  :content-keys (body format formatted_body msgtype thumbnail_url url)
  :let ( ;; We don't use `matrix-client-event-data-timestamp', because for
        ;; room messages, the origin_server_ts is the actual message time.
        (timestamp (/ origin_server_ts 1000))
        (timestamp-string (format-time-string "%T" (seconds-to-time timestamp)))
        (displayname (matrix-user-displayname room sender))
        (metadata) (msg) (matrix-image-url))
  :body (progn
          ;; (matrix-log "PROCESSING MESSAGE EVENT: \n%s" (matrix-pp-string event))
          (when content
            ;; Redacted messages have no content, so we should do nothing for them.
            (setq metadata (format$ "[$timestamp-string] $displayname> "))
            (setq message (string-trim
                           ;; Trim messages because HTML ones can have extra newlines
                           (pcase msgtype
                             ("m.emote"
                              (format$ "* $body"))
                             ((guard (and matrix-client-ng-render-html (string= "org.matrix.custom.html" format)))
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
                              (setq matrix-image-url (matrix-transform-mxc-uri session (or url thumbnail_url)))
                              (concat body
                                      ": "
                                      (matrix-client-ng-linkify-urls matrix-image-url)))
                             (_ (matrix-client-ng-linkify-urls body)))))
            ;; Apply face for own messages
            (let (metadata-face message-face)
              (cond ((equal sender user)
                     (setq metadata-face 'matrix-client-own-metadata
                           message-face 'matrix-client-own-messages))
                    ((string= msgtype "m.notice")
                     (setq metadata-face 'matrix-client-notice-metadata
                           message-face 'matrix-client-notice))
                    (t
                     (setq metadata-face 'matrix-client-metadata
                           message-face 'default)))
              ;; Use 'append so that link faces are not overridden.
              (add-face-text-property 0 (length metadata) metadata-face 'append metadata)
              (add-face-text-property 0 (length message) message-face 'append message))

            ;; Insert metadata with message and add text properties
            (matrix-client-ng-insert room (propertize (concat metadata message)
                                                      'timestamp timestamp
                                                      'displayname displayname
                                                      'sender sender
                                                      'event_id event_id))

            ;; Start image insertion if necessary
            (when matrix-client-ng-show-images
              (cl-loop for url in (-non-nil (append (matrix-client--image-urls message)
                                                    (list matrix-image-url)))
                       do (matrix-client-insert-image room event_id url)))

            ;; Move last-seen line if it's our own message
            (when (equal sender user)
              (matrix-client-ng-update-last-seen room))

            ;; Notification
            (unless (or initial-sync-p
                        (equal sender user))
              (matrix-client-notify "m.room.message" event :room room)))))

(matrix-client-ng-defevent m.room.member
  "Say that member in EVENT joined/left ROOM."
  :object-slots ((room session)
                 (session initial-sync-p))
  :event-keys (state_key)
  :content-keys (displayname membership)
  :let ((timestamp (matrix-client-ng-event-timestamp event))
        (action (pcase membership
                  ("join" "joined")
                  ("left" "left")
                  (_ membership)))
        (msg (propertize (format$ "$displayname $action")
                         'face 'matrix-client-notice
                         'event_id event_id
                         'sender sender
                         'timestamp timestamp)))
  ;; MAYBE: Get displayname from API room object's membership list.
  :body (progn
          (unless initial-sync-p
            ;; FIXME: This does not seem to work; on initial connect, "user joined" messages still show up from when the user initially joined the room.
            (matrix-client-ng-insert room msg))
          (with-room-buffer room
            ;; FIXME: It's inefficient to do this for every join event, especially on initial sync in a large room.
            (rename-buffer (matrix-client-ng-display-name room)))))

;;;; Update-room-at-once approach

(cl-defmethod matrix-client-ng-update ((room matrix-room))
  "Update ROOM."
  (with-slots* (((extra state-new timeline-new id) room))
    ;; Process new events
    (dolist (event-list (list state-new timeline-new))
      (seq-doseq (event event-list)
        (pcase-let* (((map type) event))
          (apply-if-fn (concat "matrix-client-event-" type)
              (list room event)
            (matrix-unimplemented (format$ "Unimplemented client method: $fn-name"))))))
    ;; Clear new events
    (matrix-clear-state room)
    (matrix-clear-timeline room)
    ;; TODO: Update other room things: header, avatar, typers, topic, name, aliases, etc.
    ))

(add-hook 'matrix-room-update-hook #'matrix-client-ng-update)

;;;; Helper functions

(defun matrix-client--prompt-position ()
  "Return position of prompt in current buffer."
  (ov-beg (car (ov-in 'matrix-client-prompt))))

(defun matrix--prev-property-change (pos property)
  "Return the previous position in buffer, starting from POS, where PROPERTY changes and is set.
Positions where PROPERTY is not set are ignored."
  (cl-loop do (setq pos (previous-single-property-change pos property))
           while pos
           until (get-text-property pos property)
           finally return pos))

(defun matrix--next-property-change (pos property &optional limit)
  "Return the next position in buffer, starting from POS, where PROPERTY changes and is set.
Positions where PROPERTY is not set are ignored.  If LIMIT is
non-nil, don't search past that position."
  (cl-loop do (setq pos (next-single-property-change pos property nil limit))
           while (and pos
                      (or (not limit)
                          ;; Should this be <= ?
                          (< pos limit)))
           when (get-text-property pos property)
           return pos))

(defun matrix-client-ng-event-timestamp (data)
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

(defun matrix-client-ng-linkify-urls (text)
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

(defun matrix-client-ng-buffer-visible-p (&optional buffer)
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

(defun matrix-client--update-date-headers ()
  "Update date headers in current buffer."
  (save-excursion
    (goto-char (point-min))
    (cl-loop with inhibit-read-only = t
             with limit = (matrix-client--prompt-position)
             with timestamp
             with new-header
             for pos = (matrix--next-property-change (point) 'matrix-header-day-number limit)
             while pos
             do (goto-char pos)
             for day-number = (get-text-property (point) 'matrix-header-day-number)
             when day-number
             do (progn
                  (delete-region (point) (next-single-property-change (point) 'matrix-header-day-number nil limit))
                  (matrix-client-room--insert-date-header (matrix--calendar-absolute-to-timestamp day-number))))))

;;;; Footer

(provide 'matrix-client-ng)

;;; matrix-client-ng.el ends here
