(defmacro format$ (string &rest objects)
  "Interpolated `format'.
Any word in STRING beginning with \"$\" is replaced with the
contents of the variable named that word.  OBJECTS are applied
in-order to %-sequences in STR.

For example:

  (format$ \"%s $name\" greeting)

Is expanded to:

  (format \"%s %s\" greeting name)

Variable names must contain only alphanumeric characters, -, or
_.  Any other character will be considered not part of a variable
name, which allows placing such characters adjacent to variable
names.  For example:

  (format$ \"[$date-time] %s $username>\" greeting)

Is expanded to:

  (format \"[%s] %s %s>\" date-time greeting username)"
  ;; TODO: Rewrite this using regexps, probably would be simpler.
  (cl-macrolet ((concatf (place string)
                         `(setf ,place (concat ,place ,string))))
    (cl-labels ((peek (seq)
                      (when (> (length seq) 1)
                        (elt seq 0))))
      (let* (current-var current-char current-% (new-str "") vars)
        (while (setq current-char (when (not (string-empty-p string))
                                    (prog1 (seq-take string 1)
                                      (setq string (seq-drop string 1)))))
          (pcase current-char
            ;; FIXME: Other whitespace chars.
            (" " (progn
                   (or (pcase current-%
                         (`nil nil)
                         (_ (progn
                              ;; Space after %-sequence
                              (concatf new-str current-%))))
                       (pcase current-var
                         (`nil nil)
                         (_ (progn
                              ;; Space after var
                              (push (intern current-var) vars)))))
                   (concatf new-str current-char)
                   (setq current-var nil
                         current-% nil)))
            ("%" (pcase (peek string)
                   ("%" (progn
                          ;; %%
                          (concatf new-str "%%")
                          (seq-drop string 1)))
                   (" " (progn
                          ;; % alone
                          (concatf new-str current-char)))
                   (_ (progn
                        ;; New %-sequence
                        (setq current-% current-char)
                        (push (pop objects) vars)))))
            ("$" (pcase (peek string)
                   ("$" (progn
                          ;; "$$"
                          (concatf new-str "$$")
                          (seq-drop string 1)))
                   (" " (progn
                          ;; Plain "$"
                          (concatf new-str "$")))
                   (`nil (progn
                           ;; End of string
                           (concatf new-str "$")))
                   (_ (progn
                        ;; New var
                        (concatf new-str "%s")
                        (setq current-var t)))))
            ((pred (string-match-p (rx (or alnum "-" "_"))))
             ;; Character could be part of var name or %-sequence
             (or (pcase current-%
                   (`nil nil)
                   (_ (progn
                        ;; Part of %-sequence
                        (concatf current-% current-char))))
                 (pcase current-var
                   (`nil (progn
                           ;; Non-var character
                           (concatf new-str current-char)))
                   (`t (progn
                         ;; New var name
                         (setq current-var current-char)))
                   (_ (progn
                        ;; Partial var name
                        (concatf current-var current-char))))))
            (_ (progn
                 (if (or (pcase current-%
                           (`nil nil)
                           (_ (progn
                                ;; After %-sequence
                                t)))
                         (pcase current-var
                           (`nil nil)
                           (_ (progn
                                ;; After var
                                (push (intern current-var) vars)))))
                     (progn
                       (concatf new-str current-char)
                       (setq current-var nil
                             current-% nil))
                   ;; Character not part of var name
                   (concatf new-str current-char))))))
        (cond (current-%
               ;; String ended with %-sequence
               (concatf new-str current-%))
              (current-var
               ;; String ended with variable
               (push (intern current-var) vars)))
        `(format ,new-str ,@(nreverse vars))))))

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
