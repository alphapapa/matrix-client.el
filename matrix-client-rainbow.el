(require 'rainbow-identifiers)

(defvar matrix-client-room nil)

(defgroup matrix-client-rainbow nil
  "Colorize events in room buffers according to sender.
Requires package `rainbow-identifiers'."
  :group 'matrix-client)

(defcustom matrix-client-rainbow-lightness 75
  "Lightness value for colors.
See `rainbow-identifiers-cie-l*a*b*-lightness'."
  :type 'integer)

(defcustom matrix-client-rainbow-saturation 25
  "Saturation value for colors.
See `rainbow-identifiers-cie-l*a*b*-saturation'."
  :type 'integer)

(define-minor-mode global-matrix-client-rainbow-mode
  "Colorize events in room buffers according to sender."
  :global t
  :group 'matrix-client-rainbow
  (if global-matrix-client-rainbow-mode
      (progn
        (add-hook 'matrix-client-setup-room-buffer-hook #'matrix-client-rainbow-mode)
        (dolist (session matrix-client-sessions)
          (dolist (room (oref session rooms))
            (with-room-buffer room
              (matrix-client-rainbow-mode)))))
    (remove-hook 'matrix-client-setup-room-buffer-hook #'matrix-client-rainbow-mode)
    (dolist (session matrix-client-sessions)
      (dolist (room (oref session rooms))
        (with-room-buffer room
          (matrix-client-rainbow-mode -1))))))

(define-minor-mode matrix-client-rainbow-mode
  "Colorize events in room buffers according to sender."
  :init-value nil
  (let ((fns '(matrix-client-event-m.room.message
               matrix-client-event-m.room.member
               matrix-client-event-m.room.topic
               matrix-client-event-m.room.avatar)))
    (if matrix-client-rainbow-mode
        (progn
          (unless (require 'rainbow-identifiers nil t)
            (user-error "`matrix-client-rainbow-mode' requires the package `rainbow-identifiers'"))
          (--each fns
            (advice-add it :after #'matrix-client-rainbow--room-event))
          (matrix-client-replay matrix-client-room))
      (--each fns
        (advice-remove it #'matrix-client-rainbow--room-event))
      (matrix-client-replay matrix-client-room))))

(defun matrix-client-rainbow--room-event (room event)
  "Colorize EVENT in ROOM according to sender.
To be used as :after advice to `matrix-client-event-m.room.message'."
  ;; NOTE: This doesn't colorize pending sent messages, because those don't have an event ID yet.  I
  ;; think that's okay, but maybe someday it can be fixed.  Not sure if worth the trouble.
  (with-room-buffer room
    ;; Using `when-let' avoids raising an error from weird little bugs, e.g. from text-property
    ;; positions.
    (-when-let* ((inhibit-read-only t)
                 (rainbow-identifiers-cie-l*a*b*-lightness matrix-client-rainbow-lightness)
                 (rainbow-identifiers-cie-l*a*b*-saturation matrix-client-rainbow-saturation)
                 ((&alist 'event_id event-id) event)
                 ((beg end) (matrix-client--find-propertized-string (list 'event_id event-id)))
                 (sender (get-text-property beg 'sender))
                 (hash (rainbow-identifiers--hash-function sender))
                 (face (rainbow-identifiers-cie-l*a*b*-choose-face hash)))
      (add-face-text-property beg end face))))

(provide 'matrix-client-rainbow)
