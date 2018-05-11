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

(provide 'matrix-utils)
