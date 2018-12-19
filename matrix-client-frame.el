(require 'dash)
(require 'dash-functional)
(require 'frame-purpose)

(defvar matrix-client-frame nil
  "The current Matrix Client frame.")

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
         :buffer-sort-fns (list (-on #'string< #'buffer-name)
                                (-on #'> (lambda (buffer)
                                           ;; Sort by most recent event in room.
                                           (or (when-let* ((room (buffer-local-value 'matrix-client-room buffer))
                                                           (last-event (car (oref* room timeline))))
                                                 (a-get* last-event 'origin_server_ts))
                                               0)))
                                (lambda (a b)
                                  ;; Sort modified rooms first (i.e. rooms with unseen events).
                                  (and (buffer-modified-p a)
                                       (not (buffer-modified-p b)))))
         :sidebar-buffers-fn (lambda ()
                               (cl-loop for session in matrix-client-sessions
                                        append (cl-loop for room in (oref* session rooms)
                                                        collect (oref* room client-data buffer))))
         :sidebar-auto-update nil
         :sidebar-update-on-buffer-switch t
         :require-mode nil)))

(defun matrix-client-frame-update-sidebar (&rest _ignore)
  "Update the buffer list sidebar when the `matrix-client-frame' is active.
Should be called manually, e.g. in `matrix-after-sync-hook', by
`frame-purpose--sidebar-switch-to-buffer', etc."
  (when (frame-live-p matrix-client-frame)
    (with-selected-frame matrix-client-frame
      (frame-purpose--update-sidebar))))

(provide 'matrix-client-frame)
