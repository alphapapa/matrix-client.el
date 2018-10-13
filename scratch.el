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

(cl-defmethod matrix-client-ng-room-command-cowsay ((room matrix-room) input)
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

(cl-defmethod matrix-client-ng-replay ((room matrix-room))
  "Erase and replay events into ROOM's buffer."
  (with-room-buffer room
    (let ((inhibit-read-only t)
          (matrix-client-ng-notifications nil))
      (erase-buffer)
      (cl-loop for event in (reverse (oref room timeline))
               do (matrix-client-ng-timeline room event)))))

;;; ordered-buffer

(defun matrix-client-ng--ordered-buffer-prefix-fn (timestamp)
  "FIXME"
  (let* ((ordered-buffer-header-face 'matrix-client-date-header)
         (previous-timestamp (unless (bobp)
                               (get-text-property (1- (point)) 'timestamp)))
         (day-number (time-to-days timestamp))
         (previous-day-number (when previous-timestamp
                                (time-to-days previous-timestamp))))
    (when (or (not previous-day-number)
              (not (= previous-day-number day-number)))
      (let ((ordered-buffer-header-face '(:inherit matrix-client-date-header :height 1.5))
            (ordered-buffer-header-suffix nil))
        (ordered-buffer-insert-header  (matrix-client--human-format-date timestamp)
                                       'timestamp (->> timestamp
                                                       (format-time-string "%Y-%m-%d 00:00:00")
                                                       date-to-time
                                                       time-to-seconds))))
    (when (or (not previous-timestamp)
              (>= (abs (- timestamp previous-timestamp)) matrix-client-ng-timestamp-header-delta))
      (ordered-buffer-insert-header (format-time-string "%H:%M" timestamp)
                                    'timestamp (->> timestamp
                                                    (format-time-string "%Y-%m-%d %H:%M:00")
                                                    date-to-time
                                                    time-to-seconds)))))

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
           (ordered-buffer-prefix-fn (apply-partially #'matrix-client-ng--ordered-buffer-prefix-fn timestamp))
           (ordered-buffer-point-fn (apply-partially #'ordered-buffer-point-fn
                                                     :backward-from #'point-max
                                                     :property 'timestamp
                                                     :value timestamp
                                                     :comparator #'<=)))
      (ordered-buffer-insert string 'timestamp timestamp)
      (pop-to-buffer (current-buffer)))))

(defun timestamp-overlays ()
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
