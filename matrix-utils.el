(defmacro matrix--map-merge (map &rest pairs)
  "Merge non-nil PAIRS into MAP.
PAIRS is a plist.  Any pair whose value is `nil' is ignored."
  (declare (indent defun))
  `(cl-loop for (key . value) in ,pairs by #'cddr
            when value
            do (map-put ,map key value)
            finally return ,map))

(defun matrix--alist (&rest pairs)
  "Return an alist of the key-value pairs in PAIRS whose value is non-nil.
PAIRS is a spliced plist."
  ;; e.g. (matrix--alist "direction" "b" "limit" nil) => (("direction" . "b"))
  (cl-loop for (key  value) on pairs by #'cddr
           when value
           collect (cons key value)))

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
                          (< pos limit)))
           until (get-text-property pos property)
           finally return pos))

(provide 'matrix-utils)
