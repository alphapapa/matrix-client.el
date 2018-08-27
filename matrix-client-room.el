(require 'shr)

;;;; Variables

(defvar matrix-client-insert-prefix-fn nil
  "When set, `matrix-client-ng-insert' will call this function before inserting.
Used to add a button for pending messages.")

(defvar matrix-client-ng-mode-map
  (let ((map (make-sparse-keymap))
        (mappings '(
                    "r" matrix-client-reply-or-insert
                    "R" (lambda () (interactive) (matrix-client-reply-or-insert t))
                    "RET" matrix-client-ret
                    "DEL "matrix-client-delete-backward-char
                    "M-v" matrix-client-scroll-down
                    "C-k" matrix-client-kill-line-or-unsent-message
                    "TAB" matrix-client-tab
                    )))
    (cl-loop for (key fn) on mappings by #'cddr
             do (define-key map (cl-typecase key
                                  (string (kbd key))
                                  (otherwise key))
                  fn))
    map)
  "Keymap for `matrix-client-ng-mode'.")

(defcustom matrix-client-show-room-avatars t
  "Download and show room avatars."
  :type 'boolean)

(defvar matrix-client-room-commands nil
  "List of room commands, without leading slash.
Used for completion.")

(defvar matrix-client-ng-shr-external-rendering-functions
  (a-list 'mx-reply #'matrix-client-ng--shr-mx-reply)
  "Functions used to render HTML in Matrix messages.  See `shr-external-rendering-functions'.")

;;;; Macros

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
  (declare (indent defun)
           (debug (&define symbolp stringp
                           &rest [&or [":body" def-form] [keywordp listp]])))
  (let ((method-name (intern (concat "matrix-client-event-" (symbol-name type))))
        (slots (cl-loop for (object . slots) in object-slots
                        collect (list slots object))))
    `(cl-defmethod ,method-name ((room matrix-room) event)
       ,docstring
       (declare (indent defun))
       (with-slots* ,slots
         (pcase-let* (((map content event_id sender origin_server_ts type unsigned ,@event-keys) event)
                      ((map ,@content-keys) content)
                      ,@let)
           ,body)))))

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

;;;; Commands

(defun matrix-client-scroll-down ()
  "Call `scroll-down-command'.  If point is at the top of the buffer, load history."
  (interactive)
  (if (= (line-number-at-pos (point)) 1)
      (matrix-client-ng-fetch-history matrix-client-ng-room)
    (let ((scroll-error-top-bottom t))
      (scroll-down-command))))

(defun matrix-client-kill-line-or-unsent-message (&optional message)
  "Kill current line; with prefix, kill everything after prompt."
  (interactive "P")
  (if message
      (progn
        (goto-char (matrix-client--prompt-position))
        (kill-region (point) (point-max)))
    (call-interactively #'kill-visual-line)))

(defun matrix-client-tab ()
  "If point is before prompt, move point to next event; otherwise call `indent-for-tab-command'."
  (interactive)
  (let ((prompt (matrix-client--prompt-position)))
    (if (< (point) prompt)
        (when-let ((pos (matrix-client--next-event-pos :limit prompt)))
          (goto-char pos))
      (call-interactively #'indent-for-tab-command))))

(defun matrix-client-ret ()
  "If point is before prompt, move point to prompt; otherwise call `matrix-client-send-active-line'."
  (interactive)
  (let ((prompt (matrix-client--prompt-position)))
    (if (< (point) prompt)
        (goto-char prompt)
      (call-interactively #'matrix-client-ng-send-input))))

(defun matrix-client-reply-or-insert (&optional quote-p)
  "If point is on a previous message, begin a reply addressed to its sender.  Otherwise, self-insert.
With prefix, quote message or selected region of message."
  (interactive "P")
  (if (get-text-property (point) 'sender)
      ;; Start reply
      (let ((display-name (get-text-property (point) 'displayname))
            (quote (if quote-p
                       ;; FIXME: Also quote in HTML format
                       (--> (if (use-region-p)
                                (buffer-substring (region-beginning) (region-end))
                              (matrix-client-ng--this-message))
                            (s-trim it)
                            (prog1 it
                              (remove-text-properties 0 (length it) '(read-only t) it))
                            (replace-regexp-in-string (rx bol) "> " it)
                            (concat it "\n\n"))
                     ;; Not quoting
                     ""))
            (inhibit-read-only t))
        ;; FIXME: Insert a link to username, and use a filter to transform to HTML before sending.
        (goto-char (matrix-client--prompt-position))
        (insert display-name ": " quote))
    ;; Do self-insert
    (call-interactively 'self-insert-command)))

(defun matrix-client-ng-delete-backward-char (n &optional kill-flag)
  "Delete backward unless the point is at the prompt or other read-only text."
  (interactive "p\nP")
  (unless (get-text-property (- (point) 2) 'read-only)
    (call-interactively #'delete-backward-char n kill-flag)))

(cl-defun matrix-client-ng-send-input (&key html)
  "Send current input to current room.
If HTML is non-nil, treat input as HTML."
  (interactive)
  (goto-char (matrix-client--prompt-position))
  (pcase-let* ((room matrix-client-ng-room)
               ((eieio session) room)
               ((eieio user txn-id) session)
               (input (prog1
                          (buffer-substring-no-properties (point) (point-max))
                        (delete-region (point) (point-max))))
               (first-word (when (string-match (rx bos "/" (group (1+ (not space)))) input)
                             (match-string 1 input)))
               (event-string (propertize input
                                         'sender user
                                         'timestamp (time-to-seconds)))
               (matrix-client-insert-prefix-fn (lambda ()
                                                 (insert-button "[pending] "
                                                                'face 'matrix-client-pending-messages
                                                                'action (lambda (&rest ignore)
                                                                          (when (yes-or-no-p "Resend message?")
                                                                            ;; FIXME: Include txn-id.  FIXME: This will include
                                                                            ;; the metadata, which we probably don't want to
                                                                            ;; resend.
                                                                            (matrix-send-message room string
                                                                                                 :override-txn-id (1+ txn-id))))
                                                                'help-echo "Resend message"
                                                                'transaction_id (1+ txn-id))))
               (format) (formatted-body) (extra-content))
    (when html
      (setq format "org.matrix.custom.html"
            formatted-body input
            input (matrix-client-ng--html-to-plain input)
            extra-content (a-list 'format format
                                  'formatted_body formatted-body)))
    (unless (s-blank-str? input)
      (when matrix-client-save-outgoing-messages
        (push input kill-ring))
      (apply-if-fn (concat "matrix-client-ng-room-command-" first-word)
          ;; Special command: apply command argument (i.e. without "/command ")
          (list room (s-chop-prefix (concat "/" first-word " ") input))
        (progn
          ;; Normal message
          (matrix-client-event-m.room.message
           room (a-list 'origin_server_ts (* 1000 (string-to-number (format-time-string "%s")))
                        'sender user
                        'unsigned (a-list 'transaction_id (1+ txn-id))
                        'content (a-list 'body input
                                         'msgtype "m.text"
                                         'format format
                                         'formatted_body formatted-body)
                        'type "m.room.message"))
          (matrix-send-message room input
                               :extra-content extra-content
                               :success (apply-partially #'matrix-client-send-message-callback room
                                                         ;; HACK: We have to get the txn-id
                                                         ;; ourselves here so we can apply it to the
                                                         ;; callback, before send-message returns
                                                         ;; the txn-id.
                                                         (1+ txn-id))
                               :error (apply-partially #'matrix-client-send-message-error-callback room
                                                       (1+ txn-id)))
          (matrix-client-ng-update-last-seen room))))))

(defun matrix-client-ng--html-to-plain (html)
  "Return plain-text rendering of HTML."
  ;; `shr-insert-document' insists on wrapping lines, so we disable the function it uses.
  (cl-letf (((symbol-function 'shr-fill-line) (lambda (&rest ignore) nil)))
    (let* ((tree (with-temp-buffer
                   (insert html)
                   (libxml-parse-html-region (point-min) (point-max))))
           (plain-text (with-temp-buffer
                         (shr-insert-document tree)
                         (buffer-substring-no-properties (point-min) (point-max)))))
      (s-trim plain-text))))

(cl-defmethod matrix-client-ng-upload ((room matrix-room) path)
  "Upload file at PATH to ROOM.
PATH may be a local path, optionally prefixed with \"file://\",
or a remote HTTP(S) path, in which case the file will be
downloaded and then uploaded.  Prompts for confirmation before
uploading.

Interactively, completes local file path; with prefix, reads
path/URL without completion."
  (interactive (list (if current-prefix-arg
                         (read-string "Path/URL: ")
                       (read-file-name "Upload file: " nil nil 'confirm))))
  (when (yes-or-no-p (format "Really upload %s? " path))
    (message "Uploading %s..." path)
    (matrix-upload room (pcase path
                          ;; NOTE: `url-file-local-copy' is synchronous; might be nice to do this
                          ;; with a callback.
                          ((rx bos "http" (optional "s") "://")
                           (or (url-file-local-copy path)
                               (error "Download failed (%s)" path)))
                          ((rx bos "file://" (let local-path (1+ anything)))
                           local-path)
                          (_ path)))))

;;;; Methods

(cl-defmethod matrix-client-ng-fetch-history ((room matrix-room))
  "Load earlier messages for ROOM."
  (matrix-client-ng-room-banner room "Loading history...")
  (matrix-messages room ))

(cl-defmethod matrix-client-ng-fetch-history-callback ((room matrix-room) &key data &allow-other-keys)
  (pcase-let* (((map start end chunk) data)
               (matrix-client-enable-notifications nil)) ; Silence notifications for old messages
    ;; NOTE: We don't add the events to the timeline of the room object.
    (seq-doseq (event chunk)
      (matrix-event room event))
    ;; NOTE: When direction is "b", as it is when fetching earlier messages, the "end" token is the
    ;; earliest chronologically, so it becomes the room's new "start" token.  Not confusing at
    ;; all... (maybe API 0.3.0 is better)
    (matrix-client-ng-room-banner room nil)))

(cl-defmethod matrix-client-ng-room-banner ((room matrix-room) message)
  "Display MESSAGE in a banner overlay at top of ROOM's buffer.
If MESSAGE is nil, clear existing message."
  (with-room-buffer room
    (let ((ov (or (car (ov-in 'matrix-client-banner))
                  (ov (point-min) (point-min)
                      'matrix-client-banner t)))
          (message (when message
                     (propertize message
                                 'face 'font-lock-comment-face))))
      (ov-set ov 'before-string message))))

(cl-defmethod matrix-client-ng--delete-event ((room matrix-room) plist)
  "Delete event with text properties in PLIST from ROOM's buffer."
  (with-room-buffer room
    (-when-let* (((beg end) (matrix-client-ng--find-propertized-string plist))
                 (inhibit-read-only t))
      (delete-region beg end))))

(cl-defmethod matrix-client-send-message-callback ((room matrix-room) txn-id &key data &allow-other-keys)
  "Client callback for send-message.
Replacing pending button with normal message event."
  ;; NOTE: ewoc.el might make this easier...
  (matrix-log (a-list :event 'matrix-client-send-message-callback
                      :txn-id txn-id
                      :data data))
  (pcase-let* (((eieio session) room)
               ((eieio user) session)
               ((map event_id) data)
               (inhibit-read-only t))
    (with-room-buffer room
      (-when-let* (((beg end) (matrix-client-ng--find-propertized-string (list 'transaction_id txn-id))))
        (add-text-properties beg end (list 'event_id event_id))
        ;; Remove "pending" overlay
        (--when-let (car (ov-in 'transaction_id txn-id))
          (delete-region (ov-beg it) (ov-end it))
          (delete-overlay it))))))

(cl-defmethod matrix-client-send-message-error-callback ((room matrix-room) txn-id &key data &allow-other-keys)
  "Client error callback for send-message.
Update [pending] overlay."
  ;; NOTE: ewoc.el might make this easier...
  (matrix-log (a-list :event 'matrix-client-send-message-error-callback
                      :txn-id txn-id))
  (pcase-let* (((eieio session) room)
               ((eieio user) session))
    (with-room-buffer room
      ;; MAYBE: Should probably make a little library to insert and replace things in the buffer...
      (if-let* ((inhibit-read-only t)
                ;; MAYBE: Ensure that only one overlay is found.
                (ov (car (ov-in 'transaction_id txn-id)))
                (beg (ov-beg ov))
                (end (ov-end ov)))
          (progn ;; Found message
            (delete-region beg end)
            (goto-char beg)
            ;; This should insert into the overlay
            (insert (propertize "[FAILED] "
                                'face 'matrix-client-failed-messages)))
        ;; Message not found
        (matrix-error (a-list 'event 'matrix-client-send-message-callback
                              'error "Can't find transaction"
                              :txn-id txn-id))))))

(cl-defmethod matrix-client-ng-insert ((room matrix-room) string &key update)
  "Insert STRING into ROOM's buffer.
STRING should have a `timestamp' text-property.

UPDATE may be a plist, in which case the buffer will be searched
for an existing item having text properties matching the keys and
values in UPDATE; if found, it will be replaced with STRING,
otherwise a new item will be inserted.

If `matrix-client-insert-prefix-fn' is non-nil, call that function with
point positioned before the inserted message."
  (with-room-buffer room
    (save-excursion
      (let* ((inhibit-read-only t)      ; MAYBE: use buffer-read-only mode instead
             (timestamp (get-text-property 0 'timestamp string))
             (day-number (time-to-days timestamp))
             (event-id (get-text-property 0 'event_id string))
             (non-face-properties (cl-loop for (key val) on (text-properties-at 0 string) by #'cddr
                                           unless (eq key 'face)
                                           append (list key val)))
             (string (apply #'propertize (concat string "\n") 'read-only t non-face-properties)))
        (unless (and update
                     ;; Inserting our own message, received back in /sync
                     (matrix-client-ng--replace-string update string))
          ;; Inserting someone else's message, or our own from earlier sessions
          (goto-char (matrix-client-ng--insertion-pos timestamp))
          ;; Ensure event before point doesn't have the same ID
          (when (when-let ((previous-event-pos (matrix--prev-property-change (point) 'timestamp)))
                  (string-equal event-id (get-text-property previous-event-pos 'event_id)))
            ;; FIXME: This should probably only be an error when debugging is enabled.
            (matrix-error (a-list 'event 'matrix-client-ng-insert
                                  'message "Trying to insert duplicate event"
                                  'event-id event-id)))
          (when matrix-client-insert-prefix-fn
            (funcall matrix-client-insert-prefix-fn))
          ;; Insert the message
          (insert string))
        ;; Update tracking
        (unless (matrix-client-buffer-visible-p)
          (set-buffer-modified-p t)
          (when matrix-client-use-tracking
            ;; TODO handle faces when receving highlights
            (tracking-add-buffer (current-buffer))))))))

(cl-defmethod matrix-client-ng-update-last-seen ((room matrix-room) &rest _)
  "Move the last-seen overlay to after the last message in ROOM."
  (with-room-buffer room
    ;; FIXME: Does this need to be when-let?  Shouldn't these always be found?
    (when-let ((seen-ov (car (ov-in 'matrix-client-last-seen)))
               (target-pos (1- (matrix-client--prompt-position))))
      (ov-move seen-ov target-pos target-pos))))

;;;;; Room metadata

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

;;;;; Room buffer setup

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

;;;;; Room commands

(cl-defmacro matrix-client-ng-def-room-command (name &key docstring message (msgtype "m.text") insert)
  "Define a room command that sends the return value of FN as a message.

In all expressions evaluated, the variable `room' is bound to the
room object, and `input' is bound to the command's
argument (i.e. everything after \"/command\").

MESSAGE may be a lisp expression, the value of which is sent to
the room as a message.

MSGTYPE may be, e.g. \"m.text\" (the default), \"m.emote\",
etc (see API docs).

INSERT may be a lisp expression which evaluates to a string,
which is inserted in the room buffer.  This happens after MESSAGE
is sent, if any."
  (declare (indent defun))
  (let* ((command (symbol-name name))
         (method-name (intern (concat "matrix-client-ng-room-command-" command))))
    `(progn
       (cl-defmethod ,method-name ((room matrix-room) input)
         ,docstring
         (--when-let ,message
           (matrix-send-message room it :msgtype ,msgtype))
         (--when-let ,insert
           (let ((matrix-client-insert-prefix-fn nil))
             (matrix-client-ng-insert room (matrix-client-ng--notice-string it))))
         (matrix-client-ng-update-last-seen room))
       (add-to-list 'matrix-client-room-commands ,command))))

(matrix-client-ng-def-room-command me
  :message input
  :msgtype "m.emote"
  :docstring "Send emote to room.")

(matrix-client-ng-def-room-command who
  :insert (with-slots (members) room
            (concat "Room members: "
                    (--> members
                         (--map (a-get (cdr it) 'displayname) it)
                         (--sort (string-collate-lessp it other nil 'ignore-case)
                                 it)
                         (s-join ", " it))))
  :docstring "Print list of room members.")

(matrix-client-ng-def-room-command join
  :insert (pcase-let* (((eieio session) room))
            ;; Only accept one room
            (if (> (length (s-split (rx (1+ space)) input)) 1)
                (user-error "Invalid /join command")
              (matrix-join-room session input)
              (concat "Joining room: " input)))
  :docstring "Join room on session.
INPUT should be, e.g. \"#room:matrix.org\".")

(cl-defmethod matrix-client-ng-room-command-html ((room matrix-room) input)
  "Send HTML message to ROOM.
INPUT should be, e.g. \"/html <b>...\"."
  ;; HACK: Reinsert HTML without "/html" and call send-input again
  (insert input)
  (matrix-client-ng-send-input :html t))

(matrix-client-ng-def-room-command upload
  :insert (when (matrix-client-ng-upload room input)
            (concat "Uploading: " input))
  :docstring "Upload file at local path or URL to ROOM.")

;;;; Functions

;;;;; Support

(defun matrix-client-ng--notice-string (s)
  "Return string S propertized for insertion with `matrix-client-ng-insert'.
Adds timestamp text-property at current time and sets notice face."
  (propertize s
              'timestamp (time-to-seconds)
              'face 'matrix-client-notice))

(cl-defun matrix-client--next-event-pos (&key limit backward)
  "Return position of next event in buffer.  If BACKWARD is non-nil, look backward.
If LIMIT is non-nil, don't search past it; otherwise determine
limit automatically."
  (let ((fn (cl-case backward
              ('nil #'next-single-property-change)
              (t #'previous-single-property-change)))
        (limit (or limit (cl-case backward
                           (null (matrix-client--prompt-position))
                           (t (point-min))))))
    (funcall fn (point) 'event_id nil limit)))

(defun matrix-client-ng--this-message ()
  "Return message point is on."
  (let* ((beg (previous-single-property-change (point) 'event_id))
         (end (next-single-property-change (point) 'event_id))
         ;; Skip past metadata
         (message-beg (next-single-property-change beg 'face)))
    (buffer-substring message-beg end)))

(defun matrix-client-ng--replace-string (plist string)
  "Replace text in buffer, which has text properties and values found in PLIST, with STRING.
If such text is not found, return nil."
  (save-excursion
    (goto-char (point-max))
    (-when-let* (((beg end) (matrix-client-ng--find-propertized-string plist)))
      (goto-char beg)
      (delete-region beg end)
      (insert string)
      t)))

(defun matrix-client-ng--find-propertized-string (plist)
  "Return list of beginning and ending positions in buffer that have text properties in PLIST."
  (save-excursion
    (goto-char (point-max))
    (-let* (((first-property first-value rest) (list (car plist) (cadr plist) (cddr plist))))
      (cl-loop for pos = (matrix--prev-property-change (point) first-property first-value)
               ;; NOTE: We subtract 1 from pos because
               ;; `previous-single-property-change' returns the position
               ;; *after* the property is set, so checking that position will
               ;; find no value, and then the loop will skip to where the
               ;; property *starts*.
               while pos
               when (and pos
                         (cl-loop for (property value) on rest by #'cddr
                                  always (equal (get-text-property pos property) value)))
               ;; NOTE: We assume that when the first property changes again,
               ;; we've found the beginning of the string.  To be completely
               ;; correct, we should check all of the properties and find the
               ;; first place any of them change, but that probably isn't
               ;; necessary, and it would be slower.
               return (list (previous-single-property-change pos first-property) pos)
               do (goto-char pos)))))

(defun matrix-client--update-date-headers ()
  "Update date headers in current buffer."
  (save-excursion
    (goto-char (point-min))
    (cl-loop with inhibit-read-only = t
             with limit = (matrix-client--prompt-position)
             with timestamp
             with new-header
             for pos = (matrix--next-property-change (point) 'matrix-header-day-number nil limit)
             while pos
             do (goto-char pos)
             for day-number = (get-text-property (point) 'matrix-header-day-number)
             when day-number
             do (progn
                  (delete-region (point) (next-single-property-change (point) 'matrix-header-day-number nil limit))
                  (matrix-client-room--insert-date-header (matrix--calendar-absolute-to-timestamp day-number))))))

(defun matrix-client--prompt-position ()
  "Return position of prompt in current buffer."
  (ov-beg (car (ov-in 'matrix-client-prompt))))

(defun matrix--prev-property-change (pos property &optional value limit)
  "Return the previous position in buffer, starting from POS, where PROPERTY changes and is set.
If VALUE is non-nil, ensure PROPERTY has VALUE, compared with
`equal'.  Positions where PROPERTY is not set are ignored.  If
LIMIT is non-nil, don't search before that position.  If property
doesn't change before POS, return nil."
  (cl-loop do (setq pos (previous-single-property-change pos property nil limit))
           ;; NOTE: We have to test `limit' ourselves, because `previous-single-property-change'
           ;; returns `limit' if nothing is found until it.
           while (and pos
                      (or (not limit)
                          (> pos limit)))
           for value-at-pos = (or (get-text-property pos property)
                                  ;; HACK: We also check the value at the position before the change is detected, because
                                  ;; `previous-single-property-change' returns the position after it changes, where it has
                                  ;; no value.  But we only do this when testing for a value.
                                  (when value
                                    (get-text-property (1- pos) property)))
           when (and value-at-pos
                     (or (not value)
                         (equal value-at-pos value)))
           return pos))

(defun matrix--next-property-change (pos property &optional value limit)
  "Return the next position in buffer, starting from POS, where PROPERTY changes and is set.
If VALUE is non-nil, ensure PROPERTY has VALUE, compared with
`equal'.  Positions where PROPERTY is not set are ignored.  If
LIMIT is non-nil, don't search past that position.  If property
doesn't change after POS, return nil."
  (cl-loop do (setq pos (next-single-property-change pos property nil limit))
           ;; NOTE: We have to test `limit' ourselves, because `next-single-property-change' returns
           ;; `limit' if nothing is found until it.
           while (and pos
                      (or (not limit)
                          ;; Should this be <= ?
                          (< pos limit)))
           for value-at-pos = (get-text-property pos property)
           when (and value-at-pos
                     (or (not value)
                         (equal value-at-pos value)))
           return pos))

(defun matrix-client-ng--propertize-buffer-string (find-plist set-plist)
  "Find string in buffer having text properties in FIND-PLIST, then add the properties in SET-PLIST.
If string is not found or no properties change, return nil."
  (-when-let* (((beg end) (matrix-client-ng--find-propertized-string find-plist))
               (inhibit-read-only t))
    (add-text-properties beg end set-plist)))

(defun matrix-client-ng--insertion-pos (timestamp)
  "Return insertion position for TIMESTAMP.
Creates a new header if necessary."
  (let* ((header-pos (matrix-client--get-date-header timestamp))
         (limit (or (matrix--next-property-change header-pos 'matrix-header-day-number)
                    (1- (matrix-client--prompt-position))))
         (next-timestamp-pos (matrix--next-property-change header-pos 'timestamp nil limit)))
    (catch 'found
      (while next-timestamp-pos
        (when (>= (get-text-property next-timestamp-pos 'timestamp) timestamp)
          ;; Found greater timestamp: return its position
          (throw 'found next-timestamp-pos))
        ;; Look for next timestamp
        (setq next-timestamp-pos (matrix--next-property-change next-timestamp-pos 'timestamp nil limit)))
      ;; No more timestamps: return limit
      limit)))

(defun matrix-client--get-date-header (timestamp)
  "Return position of appropriate date header in current buffer for TIMESTAMP.
Creates a new header if necessary."
  (cl-labels ((prev-header-pos () (matrix--prev-property-change (point) 'matrix-header-day-number))
              (current-header-day-number () (get-text-property (point) 'matrix-header-day-number)))
    (let* ((target-day-number (time-to-days timestamp))
           (prompt (1- (matrix-client--prompt-position)))
           (inhibit-read-only t))
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
                         (goto-char (or (matrix--next-property-change (point) 'matrix-header-day-number nil prompt)
                                        prompt))
                         (matrix-client--update-date-headers)
                         (matrix-client-room--insert-date-header timestamp)
                         ;; Return position after new header (actually 1- it, see below)
                         (throw 'found (1- (point))))))
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

;;;; Events

(defun matrix-client-ng--shr-mx-reply (dom)
  "Insert formatted text for DOM rooted at mx-reply tag.
The purpose of this function is to add the
`matrix-client-quoted-message' face to only the quoted message
body, rather than the entire contents of the mx-reply tag (which
includes the \"In reply to\" link to the quoted message ID)."
  ;; NOTE: As long as the Matrix server sends formatted_body with mx-reply tags in this format, this
  ;; should work.

  ;; TODO: Suggest that the Matrix server should send the quoted message as event metadata rather
  ;; than pseudo-HTML.  Then we wouldn't have to do this hacky parsing of the pseudo HTML.

  ;; NOTE: `-let' makes destructuring the DOM pretty easy, but walking it and replacing parts of it
  ;; is very messy, because we sometimes replace strings with lists, which must then be flattened
  ;; and spliced back into the DOM.  If you're reading this and you can make this code prettier,
  ;; please do.
  (cl-labels ((newline-to-br (string)
                             ;; I couldn't find an existing function to split a string by a regexp
                             ;; AND replace the matches with other elements of a number equal to the
                             ;; length of each match, so I came up with this.
                             (cl-loop with length = (length string)
                                      with positions = (append (s-matched-positions-all "\n+" string)
                                                               (list (cons length length)))
                                      with from = 0
                                      for (match-start . match-end) in positions
                                      collect (substring string from match-start)
                                      append (-repeat (- match-end match-start) '(br nil))
                                      do (setq from match-end)))
              (walk-dom (dom)
                        (pcase dom
                          (`(,tag ,props . ,children) `((,tag ,props ,@(-flatten-n 1 (mapcar #'walk-dom children)))))
                          ((rx bos "\n" eos) '((br nil)))
                          ((pred stringp) (if (s-contains? "\n" dom)
                                              (newline-to-br dom)
                                            ;; Always return a list so we can flatten it.  This is really messy.  Ugh.
                                            (list dom))))))
    (-let* (((_mx-reply-tag _mx-reply-tag-props
                            (_blockquote-tag _blockquote-tag-props
                                             quoted-event-a
                                             _blank-space
                                             quoted-sender-a
                                             _br-tag
                                             . quoted-dom))
             dom)
            (quoted-dom (-flatten-n 1 (mapcar #'walk-dom quoted-dom)))
            (dom `(html nil (body nil (blockquote nil ,@quoted-dom)))))
      (shr-tag-a quoted-event-a) (insert " ") (shr-tag-a quoted-sender-a) (insert ":")
      (let ((pos (point)))
        ;; NOTE: `shr' folds newlines
        (shr-insert-document dom)
        (add-face-text-property pos (point) 'matrix-client-quoted-message)
        ;; Insert extra newline after blockquote.  (I think shr doesn't insert a blank line after
        ;; the blockquote because it doesn't see anything after the blockquote.)
        (insert "\n")))))

(matrix-client-ng-defevent m.room.message
  "Process m.room.message EVENT in ROOM."
  :object-slots ((room session)
                 (session user initial-sync-p))
  :content-keys (body format formatted_body msgtype thumbnail_url url)
  :let (;; We don't use `matrix-client-event-data-timestamp', because for
        ;; room messages, the origin_server_ts is the actual message time.
        (timestamp (/ origin_server_ts 1000))
        ;; FIXME: Not sure we need to call `seconds-to-time' here.
        (timestamp-string (format-time-string "%T" (seconds-to-time timestamp)))
        (displayname (matrix-user-displayname room sender))
        ((map transaction_id) unsigned)
        (metadata) (msg) (matrix-image-url))
  :body (progn
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
                                (let* ((shr-external-rendering-functions matrix-client-ng-shr-external-rendering-functions)
                                       (dom (libxml-parse-html-region (point-min) (point-max))))
                                  (erase-buffer)
                                  (shr-insert-document dom))
                                (buffer-string)))
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
                           message-face 'matrix-client-own-message-body))
                    ((string= msgtype "m.notice")
                     (setq metadata-face 'matrix-client-notice-metadata
                           message-face 'matrix-client-notice))
                    (t
                     (setq metadata-face 'matrix-client-metadata
                           message-face 'matrix-client-message-body)))
              ;; Use 'append so that link faces are not overridden.
              (add-face-text-property 0 (length metadata) metadata-face 'append metadata)
              (add-face-text-property 0 (length message) message-face 'append message))

            ;; Delete existing event, and insert metadata with message and add text properties
            (when event_id
              (matrix-client-ng--delete-event room (list 'event_id event_id)))
            (matrix-client-ng-insert room (propertize (concat metadata message)
                                                      'timestamp timestamp
                                                      'displayname displayname
                                                      'sender sender
                                                      'event_id event_id
                                                      'transaction_id (cl-typecase transaction_id
                                                                        (number transaction_id)
                                                                        ;; The server treats txn-ids as strings.
                                                                        (string (string-to-number transaction_id)))))

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
  :event-keys (state_key sender)
  :content-keys (displayname membership)
  :let ((displayname (or displayname sender))
        (timestamp (matrix-client-ng-event-timestamp event))
        (action (pcase membership
                  ("join" "joined")
                  ("leave" "left")
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

(matrix-client-ng-defevent m.typing
  "Handle m.typing events."
  :object-slots ((room typers))
  :content-keys (user_ids)
  :body (progn
          (setq typers user_ids)
          (matrix-client-ng-update-header room)))

(matrix-client-ng-defevent m.room.topic
  "Handle m.room.topic events."
  :object-slots ((room topic))
  :body (--when-let (a-get content 'topic)
          ;; We can't use :content-keys to get the topic, because it shadows the room slot.
          (setq topic it)
          (matrix-client-ng-update-header room)))

(matrix-client-ng-defevent m.room.avatar
  "Handle room avatar events."
  :object-slots ((room avatar session)
                 (session initial-sync-p user))
  :content-keys (sender url)
  :let ((username (matrix-user-displayname room sender))
        (own-username (matrix-user-displayname room user))
        (timestamp (matrix-client-ng-event-timestamp event))
        (time-string (format-time-string "[%T]" (seconds-to-time timestamp)))
        (action (if url "changed" "removed"))
        (message (unless initial-sync-p
                   (propertize (format "%s %s %s the room avatar" time-string username action)
                               'timestamp timestamp
                               'face 'matrix-client-notice))))
  :body (when matrix-client-show-room-avatars
          (if url
              ;; New avatar
              ;; TODO: Maybe display the new avatar in the chat list, like Riot.
              (matrix-url-with-retrieve-async (matrix-transform-mxc-uri session url)
                :parser (apply-partially #'matrix-client-parse-image room :max-width 32 :max-height 32)
                :success (apply-partially #'matrix-client-room-avatar-callback
                                          :room room
                                          :message message
                                          :max-width 32
                                          :max-height 32))
            ;; Avatar removed
            (setq avatar nil)
            ;; TODO: A function to automatically propertize a string with its related event data would be nice.
            (when message
              (matrix-client-ng-insert room message))
            (matrix-client-ng-update-header room))

          ;; Move last-seen line if it's our own message
          (when (equal own-username username)
            (matrix-client-ng-update-last-seen room))))

(cl-defmethod matrix-client-room-avatar-callback (&key (room matrix-room) message data &allow-other-keys)
  "Set avatar for ROOM.
Image is passed from parser as DATA, which should be an image
object made with `create-image'.  This function should be called
as an async callback when the image is downloaded."
  (with-slots (avatar) room
    (when-let ((image-string (with-temp-buffer
                               (insert " ")
                               (insert-image data)
                               (insert " ")
                               (buffer-string))))
      (setq avatar image-string)
      (matrix-client-ng-update-header room)
      (when message
        (matrix-client-ng-insert room message)))))

;;;; Update-room-at-once approach

(cl-defmethod matrix-client-ng-update ((room matrix-room))
  "Update ROOM."
  (with-slots* (((extra state-new timeline-new ephemeral id) room))
    ;; Process new timeline events
    (dolist (event-list (list state-new timeline-new))
      (seq-doseq (event event-list)
        (pcase-let* (((map type) event))
          (apply-if-fn (concat "matrix-client-event-" type)
              (list room event)
            (matrix-unimplemented (format$ "Unimplemented client method: $fn-name"))))))
    ;; Clear new events
    (matrix-clear-state room)
    (matrix-clear-timeline room)
    ;; Process new ephemeral events
    (seq-doseq (event ephemeral)
      (pcase-let* (((map type) event))
        (apply-if-fn (concat "matrix-client-event-" type)
            (list room event)
          (matrix-unimplemented (format$ "Unimplemented client method: $fn-name")))))
    (setq ephemeral nil)                ; I think we can skip making a method for this.
    ;; TODO: Update other room things: header, avatar, typers, topic, name, aliases, etc.
    (matrix-client-ng-room-banner room nil)
    ))

(add-hook 'matrix-room-update-hook #'matrix-client-ng-update)

;;;; Footer

(provide 'matrix-client-room)
