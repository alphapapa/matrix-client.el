(matrix-defclass argh-test nil
  ((extra :initarg :extra)))

;;;; Works

(let ((room (argh-test)))
  ;; WORKS
  (with-slots* (((extra) room))
    (let* ((buffer (a-get extra 'buffer)))
      (unless buffer
        (map-put extra 'buffer "BUFFER NAME"))))
  (oref room extra))

(let ((room (argh-test)))
  ;; WORKS
  (with-slots* (((extra) room))
    (pcase-let* (((map buffer) extra))
      (unless buffer
        (map-put extra 'buffer "BUFFER NAME"))))
  (oref room extra))

;;;; Doesn't work

(let ((room (argh-test)))
  ;; DOES NOT WORK
  (pcase-let* (((eieio extra) room)
               ((map buffer) extra))
    (unless buffer
      (map-put extra 'buffer "BUFFER NAME")))
  (oref room extra))

(let ((room (argh-test)))
  ;; DOES NOT WORK
  (pcase-let* (((eieio extra) room)
               ((map buffer) extra))
    (unless buffer
      (setf buffer "BUFFER NAME")))
  (oref room extra))

;;; derp

(defun matrix-client-room-command-cowsay (room input)
  "Cowsay!"
  (let* ((s (replace-regexp-in-string (rx bos "/" (1+ (not space)) (1+ space)) "" input))
         (cow-type (seq-random-elt '("-b" "-d" "-g" "-p" "-s" "-t" "-w" "-y")))
         (cowsaid (shell-command-to-string (format "cowsay %s %s" cow-type (shell-quote-argument s))))
         (html (concat "<font color=\"#000\" data-mx-color=\"#000\" data-mx-bg-color=\"#fff\"><pre>\n"
                       "<strong>" (htmlize-escape-or-link cowsaid) "</strong>"
                       "</pre></font>")))
    (matrix-send-message room (concat "Cow: \"" s "\"")
                         :extra-content (a-list 'formatted_body html
                                                'format "org.matrix.custom.html"))))

;; (defun cowsay (s)
;;   (shell-command-to-string (format "cowsay %s" (shell-quote-argument s))))

;;; Replay room

(defun matrix-client-replay (room)
  "Erase and replay events into ROOM's buffer."
  (with-room-buffer room
    (let ((inhibit-read-only t)
          (matrix-client-notifications nil))
      (ov-clear)
      (erase-buffer)
      (matrix-client-insert-prompt room)
      (matrix-client-insert-last-seen room)
      (cl-loop for event in (reverse (oref room timeline))
               do (matrix-client-timeline room event)))))

;;; ordered-buffer

(defun ordered-buffer-test (&optional timestamp)
  (interactive (list (cond (current-prefix-arg (- (string-to-number (format-time-string "%s"))
                                                  (read-number "Seconds before now: ")))
                           (t (string-to-number (format-time-string "%s"))))))
  (with-current-buffer (get-buffer-create "*ordered-buffer-test*")
    (let* ((string (concat (format-time-string "%H:%M:%S" timestamp)
                           " "
                           (s-trim (shell-command-to-string "cat /usr/share/dict/words | shuf -n1"))
                           "\n"))
           (inhibit-read-only t)
           (ordered-buffer-prefix-fn (apply-partially #'matrix-client--ordered-buffer-prefix-fn timestamp))
           (ordered-buffer-point-fn (apply-partially #'ordered-buffer-point-fn
                                                     :backward-from #'point-max
                                                     :property 'timestamp
                                                     :value timestamp
                                                     :comparator #'<=)))
      (ordered-buffer-insert string 'timestamp timestamp)
      (pop-to-buffer (current-buffer)))))

(defun timestamp-overlays ()
  "Display overlays in margin in current buffer indicating `timestamp' text-property on each line.
For debugging."
  (interactive)
  (setq left-margin-width 40)
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (ov-clear :timestamp-overlay)
      (cl-loop for ts = (get-text-property (point) 'timestamp)
               when ts
               do (ov (point) (or (next-single-property-change (point) 'timestamp)
                                  (point-max))
                      'before-string (propertize "o"
                                                 'display (list '(margin left-margin)
                                                                (concat (number-to-string ts)
                                                                        " "
                                                                        (format-time-string "%Y-%m-%d %H:%M:%S" ts))))
                      :timestamp-overlay t)
               for next = (next-single-property-change (point) 'timestamp)
               if next
               do (goto-char next)
               else return nil))))

(defun hl-event (event-id)
  "Highlight the event with EVENT-ID in the current buffer."
  (-when-let* (((beg end) (matrix-client--find-propertized-string (list 'event_id event-id))))
    (goto-char beg)
    (set-mark end)))
