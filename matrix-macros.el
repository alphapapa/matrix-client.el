(defmacro oref* (&rest slots)
  "Access SLOTS of nested EIEIO objects.
The first of SLOTS should be an object, while the rest should be
slot symbols.  Accessing each slot should return an object for
which the next slot is valid, except for the last slot, which may
return any value."
  (cl-labels ((rec (slots)
                   `(oref ,(if (and (consp (cdr slots))
                                    (cddr slots))
                               (rec (cdr slots))
                             (cadr slots))
                          ,(car slots))))
    (rec (nreverse slots))))

(defmacro a-get* (&rest keys)
  ;; See https://github.com/plexus/a.el/issues/7
  (cl-labels ((rec (keys)
                   `(a-get ,(if (and (consp (cdr keys))
                                     (cddr keys))
                                (rec (cdr keys))
                              (cadr keys))
                           ,(car keys))))
    (rec (nreverse keys))))

(defmacro with-slots* (slots-objects &rest body)
  "Access slots of nested objects, evaluating BODY.
Creates nested `with-slots' forms, so each slot is a generalized
variable.  For example:

\(with-slots* (((id session) room)
              ((user) session))
             user)

Is transformed to:

\(with-slots (id session) room
  (with-slots (user) session
    user))"
  (declare (debug (listp body))
           (indent defun))
  (cl-loop for (slots object) in (reverse slots-objects)
           do (setq body `((with-slots ,slots ,object ,@body)))
           finally return (car body)))

(provide 'matrix-macros)
