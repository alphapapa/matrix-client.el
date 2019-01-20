;; -*- lexical-binding: t; -*-


(require 'subr-x)
(require 'url-http)

(defmacro if-fn-apply (fn-name args else)
  "If FN-NAME is a function, return result of applying ARGS to it, otherwise eval ELSE form.
FN-NAME should be a string, and is available in the ELSE form as `fn-name'."
  (declare (debug (form listp form)))
  ;; FIXME: Probably use with-gensyms* here.
  `(let* ((fn-name ,fn-name)
          (fn (intern-soft ,fn-name)))
     (if (functionp fn)
         (apply fn ,args)
       ,else)))
(put 'if-fn-apply 'lisp-indent-function 2)

(defmacro format$ (string &rest objects)
  "Interpolated `format'.
Any word in STRING beginning with \"$\" is replaced with the
contents of the variable named that word.  OBJECTS are applied
in-order to %-sequences in STR.  Words surrounded by \"${}\" may
contain %-sequences.

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

  (format \"[%s] %s %s>\" date-time greeting username)

Including %-sequences, this:

  (format$ \"Amount: ${amount%.02f}  $name  %s\" date)

Expands to:

  (format \"Amount: %.02f  %s  %s\" amount name date)"
  (cl-macrolet ((concatf (place string)
                         `(setf ,place (concat ,place ,string)))
                (peek (seq)
                      `(when (> (length ,seq) 1)
                         (seq-take ,seq 1))))
    (let* (current-var current-char current-% current-{ (new-str "") vars)
      (while (setq current-char (when (not (string-empty-p string))
                                  (prog1 (seq-take string 1)
                                    (setq string (seq-drop string 1)))))
        (pcase current-char
          ;; FIXME: Other whitespace chars.  (Use pcase rx matcher in Emacs 26!)
          (" " (progn
                 (or (pcase current-%
                       (`nil nil)
                       (_ (pcase current-{
                            (`t (progn
                                  ;; Space as part of %-sequence
                                  (concatf current-% current-char)))
                            (_ (progn
                                 ;; Space after %-sequence
                                 (concatf new-str current-%))))))
                     (pcase current-var
                       (`nil nil)
                       (_ (progn
                            ;; Space after var
                            (push (intern current-var) vars)))))
                 (unless current-{
                   (concatf new-str current-char)
                   (setq current-var nil
                         current-% nil))))
          ("%" (pcase (peek string)
                 ("%" (progn
                        ;; %%
                        (concatf new-str "%%")
                        (seq-drop string 1)))
                 (" " (pcase current-{
                        (`t (progn
                              ;; Part of %-sequence
                              (setq current-% current-char)))
                        (_ (progn
                             ;; % alone
                             (concatf new-str current-char)))))
                 (_ (progn
                      ;; New %-sequence
                      (setq current-% current-char)
                      (unless current-{
                        (push (pop objects) vars))))))
          ("$" (pcase (peek string)
                 ("$" (progn
                        ;; "$$"
                        (concatf new-str "$$")
                        ;; FIXME: Using seq-drop here seems incorrect
                        (seq-drop string 1)))
                 (" " (progn
                        ;; Plain "$"
                        (concatf new-str "$")))
                 (`nil (progn
                         ;; End of string
                         (concatf new-str "$")))
                 ("{" (progn
                        ;; New variable with % control string
                        (setq current-var t
                              current-{ t)
                        (setq string (seq-drop string 1))))
                 (_ (progn
                      ;; New var
                      (concatf new-str "%s")
                      (setq current-var t)))))
          ((pred (string-match-p (rx (or alnum "-" "_" "." "+" "#"))))
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
          ("}" (progn
                 (if (and current-var current-%)
                     (progn
                       ;; Closing ${} sequence
                       (push (intern current-var) vars)
                       (concatf new-str current-%)
                       (setq current-var nil
                             current-% nil
                             current-{ nil))
                   ;; Plain }
                   (concatf new-str current-char))))
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
      `(format ,new-str ,@(nreverse vars)))))

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

(defvar-local matrix-url-with-retrieve-async-timeout-timer nil
  "When a response buffer has a timeout, this variable stores the
  timer object so that it may be canceled if the request
  completes successfully.")

(cl-defun matrix-url-with-retrieve-async (url &key cbargs silent inhibit-cookies data
                                              (method "GET") extra-headers query timeout success error
                                              parser (query-on-exit t))
  "Retrieve URL asynchronously with `url-retrieve'.

Arguments CBARGS, SILENT, and INHIBIT-COOKIES are passed to
`url-retrieve', which see.

DATA is bound to `url-request-data', which see.

METHOD may be a symbol or string, which is bound as a capitalized
string to `url-request-method', which see.

EXTRA-HEADERS is an alist of header-value pairs, which is bound
to `url-request-extra-headers', which see.

QUERY is an alist of key-value pairs which is appended to the URL
as the query.

TIMEOUT may be a number of seconds, after which the error
callback will run if the request hasn't completed by then.

SUCCESS may be a function symbol or a body form, which is called
with zero arguments upon successful completion of the request.
In the call to SUCCESS, these variables will be bound:

`status': See `url-retrieve'.
`cbargs': See `url-retrieve'.
`headers': The HTTP response headers as a string.
`body': The HTTP response body as a string.

ERROR may be a function symbol or a body form, which is called
with zero arguments if the request fails.  In the error call,
these variables will be bound, in addition to the ones bound for
SUCCESS:

`errors': The list of `url' error symbols for the most recent
error, e.g. `(error http 404)' for an HTTP 404 error.

In the SUCCESS and ERROR calls, the current buffer is the
response buffer, and it is automatically killed when the call
completes.

PARSE-BODY-FN may be a function which parses the body and returns
a value to bind `body' to.  The point is positioned after the
headers, at the beginning of the body, before calling the
function.  For example, `json-read' may be used to parse JSON
documents, after which the parsed JSON would be available in
SUCCESS and ERROR as `body'.  Or, if the body is not needed,
`ignore' could be used to prevent the body from being parsed."
  (declare (indent defun))
  (let* ((success-body-fn (cl-typecase success
                            (function success)
                            (otherwise (byte-compile
                                        `(cl-function
                                          (lambda (&key cbargs status headers data)
                                            ,success))))))
         (error-body-fn (cl-typecase error
                          (function error)
                          (otherwise (byte-compile
                                      `(cl-function
                                        (lambda (&key cbargs status error headers data url)
                                          ,error))))))
         (url-request-data (when data
                             (encode-coding-string data 'utf-8)))
         (url-request-method (upcase (cl-typecase method
                                       (symbol (symbol-name method))
                                       (string method))))
         ;; TODO: Note that extra-headers must be an alist, and both keys and values must be strings.
         (url-request-extra-headers extra-headers)
         ;; FIXME: Document how `url-http-attempt-keepalives' is set.
         (url-http-attempt-keepalives (and (not timeout)
                                           url-http-attempt-keepalives))
         (callback (lambda (status &optional cbargs)
                     (unwind-protect
                         ;; This is called by `url-http-activate-callback' with the response buffer
                         ;; as the current buffer.

                         ;; Check for errors
                         (pcase status
                           ;; NOTE: This may need to be updated to correctly handle multiple errors
                           (`(:error . ,_) (funcall error-body-fn
						    :url url
                                                    :cbargs cbargs
                                                    :status status
                                                    :error (plist-get status :error)))
                           ((or 'nil
                                `(:peer . ,_)
                                `(:redirect . ,_))
                            (if (not url-http-end-of-headers)
                                ;; HACK: It seems that the callback can be called with `nil' when
                                ;; the connection fails before getting any headers, like:
                                ;; url-http-end-of-document-sentinel(#<process matrix.org<5>>
                                ;; "connection broken by remote peer\n"), in which case
                                ;; `url-http-end-of-headers' is nil, so we need to call the error
                                ;; fn.  Would like to structure this more cleanly.
                                (funcall error-body-fn
                                         :url url
                                         :cbargs cbargs
                                         :status status
                                         :error (plist-get status :error))
                              (let ((headers (buffer-substring (point) url-http-end-of-headers))
                                    (data (if parser
                                              (progn
                                                (goto-char (1+ url-http-end-of-headers))
                                                (funcall parser))
                                            (buffer-substring (1+ url-http-end-of-headers) (point-max)))))
                                (funcall success-body-fn
                                         :cbargs cbargs
                                         :status status
                                         :headers headers
                                         :data data))))
                           ;; NOTE: This "please report" thing actually worked!  See issue #69.
                           (_ (error "Response status unrecognized; please report this error: %s" (pp-to-string status))))
                       (when matrix-url-with-retrieve-async-timeout-timer
                         (cancel-timer matrix-url-with-retrieve-async-timeout-timer))
                       (unless (kill-buffer (current-buffer))
                         (warn "Unable to kill response buffer: %s" (current-buffer))))))
         url-obj query-string query-params response-buffer)
    (when (or (memq 'http url-debug)
              (eq url-debug t))
      (matrix-log (a-list 'type 'url-with-retrieve-async
                          'method method
                          'url url
                          'query query
                          'extra-headers extra-headers
                          'data data
                          'timeout timeout
                          'parser parser
                          'success success
                          'error error)))
    (when query
      ;; Build and append query string to URL
      (progn
        ;; Transform alist to plain list for `url-build-query-string'
        (setq query-params (cl-loop for (key . val) in query
                                    when val
                                    collect (list key val)))
        (setq url-obj (url-generic-parse-url url))
        (setq query-string (url-build-query-string query-params))
        (setf (url-filename url-obj) (concat (url-filename url-obj) "?" query-string))
        (setq url (url-recreate-url url-obj))))
    (setq response-buffer (url-retrieve url callback cbargs silent inhibit-cookies))
    (when timeout
      (with-current-buffer response-buffer
        (setq-local matrix-url-with-retrieve-async-timeout-timer
                    (run-with-timer timeout nil
                                    (lambda ()
                                      (when (and (buffer-live-p response-buffer)
                                                 (get-buffer-process response-buffer))
                                        (with-current-buffer response-buffer
                                          ;; Since we are handling the timeout ourselves, when we kill the
                                          ;; process, url.el considers it a "success", and therefore does not kill
                                          ;; the buffer (it seems to only kill its own buffers when it detects a
                                          ;; HTTP response error code, which we aren't getting).  So we first add
                                          ;; an errors list to the first element of the callback args (the
                                          ;; `status' arg), then we delete the process, causing the process's
                                          ;; sentinel to be called, which then calls the callback, which detects
                                          ;; the error and calls the error-body-fn.

                                          ;; FIXME: Sometimes this seems to stop catching timeouts.
                                          ;; When that happens, it seems that the response buffer
                                          ;; process does not get deleted, as it remains listed in
                                          ;; `list-processes'.  Maybe the solution is to bind
                                          ;; `url-http-attempt-keepalives' to nil when a timeout is
                                          ;; set, because maybe that would prevent processes from
                                          ;; being left around, which seems to contribute to the
                                          ;; problem.

                                          ;; NOTE: This may be loosely relevant: <https://github.com/jorgenschaefer/circe/issues/327>
                                          (setq url-callback-arguments (list (list :error 'timeout) url-callback-arguments))
                                          ;; Since `get-buffer-process' is a C function, we just call it again
                                          ;; instead of storing the buffer process in a variable.
                                          (delete-process (get-buffer-process response-buffer))
                                          (setq matrix-url-with-retrieve-async-timeout-timer nil))))))))
    (unless query-on-exit
      (set-process-query-on-exit-flag (get-buffer-process response-buffer) nil))
    response-buffer))

(defmacro with-room-buffer (_room &rest body)
  ;; FIXME: This macro uses the value of `room' from the surrounding environment
  ;; rather than the symbol passed to the macro.  This should be fixed someday.

  ;; NOTE: Don't move this macro to another file.  It's not worth it.  Trust me.
  (declare (debug (sexp body)) (indent defun))
  `(with-slots* (((client-data id display-name) room)
                 ((buffer) client-data))
     (unless (and buffer (buffer-live-p buffer))
       ;; Make buffer if necessary.  This seems like the easiest way
       ;; to guarantee that the room has a buffer, since it seems
       ;; unclear what the first received event type for a joined room
       ;; will be.
       (setq buffer (generate-new-buffer (or display-name
                                             (oset room display-name (matrix--room-display-name room))
                                             ;; "*matrix-room temporary buffer name*"
                                             )))
       (matrix-client-setup-room-buffer room))
     (with-current-buffer buffer
       ,@body)))

(provide 'matrix-macros)
