;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'eieio)
(require 'subr-x)

(require 'a)
(require 'dash)
(require 'dash-functional)
(require 'frame-purpose)

(defvar matrix-client-frame nil
  "The current Matrix Client frame.")

;; TODO: It'd be nice to byte-compile these comparators, because it doesn't seem to happen just by
;; byte-compiling the file.
(defcustom matrix-client-frame-sort-fns
  '(matrix-client-room-buffer-favorite<
    matrix-client-room-buffer-name<)
  "How to sort room buffers in the frame sidebar.
A list of functions that take two room buffers as arguments and
return non-nil if the first should be sorted before the second."
  :group 'matrix-client
  :type `(repeat (choice (const :tag "Favourite rooms" matrix-client-room-buffer-favorite<)
                         (const :tag "Room name" matrix-client-room-buffer-name<)
                         (const :tag "Most recent event in room" matrix-client-room-buffer-latest-event<)
                         (const :tag "Unseen events" matrix-client-room-buffer-unseen-events<))))

(defun matrix-client-room-buffer-favorite< (buffer-a buffer-b)
  "Return non-nil if BUFFER-A's room is a favorite and BUFFER-B's is not."
  (cl-macrolet ((room-favorite-p
                 (buffer)
                 `(with-slots (tags) (buffer-local-value 'matrix-client-room ,buffer)
                    (assq 'm.favourite tags))))
    (and (room-favorite-p buffer-a)
         (not (room-favorite-p buffer-b)))))

(defun matrix-client-room-buffer-name< (buffer-a buffer-b)
  "Return non-nil if BUFFER-A's room name is `string<' than BUFFER-B's."
  (string< (buffer-name buffer-a)
           (buffer-name buffer-b)))

(defun matrix-client-room-buffer-latest-event< (buffer-a buffer-b)
  "Return non-nil if BUFFER-A's room's latest event is more recent than BUFFER-B's."
  (> (matrix-client-buffer-latest-event-ts buffer-a)
     (matrix-client-buffer-latest-event-ts buffer-b)))

(defun matrix-client-room-buffer-unseen-events< (buffer-a buffer-b)
  "Return non-nil if BUFFER-A's room is modified but not BUFFER-B's."
  (and (buffer-modified-p buffer-a)
       (not (buffer-modified-p buffer-b))))

;; This function can stay here for now since it's only used here.
(defun matrix-client-buffer-latest-event-ts (buffer)
  "Return timestamp of latest event in BUFFER's room."
  (when-let* ((room (buffer-local-value 'matrix-client-room buffer))
              (last-event (car (oref* room timeline))))
    (a-get* last-event 'origin_server_ts)))

;;;###autoload
(defun matrix-client-frame (&optional side)
  "Open and return the Matrix Client frame on SIDE.
SIDE may be `left', `right', `top', or `bottom'.

Only one such frame should be open at a time.  If more than one
is, only the latest one will have its sidebar updated
automatically."
  (interactive (list (if current-prefix-arg
                         (intern (completing-read "Side: " '(left right top bottom)))
                       'right)))
  (matrix-client-connect)
  (add-hook 'matrix-after-sync-hook #'matrix-client-frame-update-sidebar)
  (setq matrix-client-frame
        (frame-purpose-make-frame
         :modes '(matrix-client-mode)
         :title "Matrix"
         :icon-type (expand-file-name "logo.png" (file-name-directory (locate-library "matrix-client-frame")))
         :sidebar side
         :buffer-sort-fns matrix-client-frame-sort-fns
         :sidebar-buffers-fn (lambda ()
                               (cl-loop for session in matrix-client-sessions
                                        append (cl-loop for room in (oref* session rooms)
                                                        collect (oref* room client-data buffer))))
         :sidebar-auto-update nil
         :sidebar-update-on-buffer-switch t
         :sidebar-header " Rooms"
         :require-mode nil)))

(defun matrix-client-frame-update-sidebar (&rest _ignore)
  "Update the buffer list sidebar when the `matrix-client-frame' is active.
Should be called manually, e.g. in `matrix-after-sync-hook', by
`frame-purpose--sidebar-switch-to-buffer', etc."
  (when (frame-live-p matrix-client-frame)
    (with-selected-frame matrix-client-frame
      (frame-purpose--update-sidebar))))

(provide 'matrix-client-frame)
