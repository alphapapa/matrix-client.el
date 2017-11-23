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
