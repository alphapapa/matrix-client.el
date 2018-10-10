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
