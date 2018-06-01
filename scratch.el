;; -*- lexical-binding: t; -*-


(defun matrix-client-reconnect (arg)
  "Reconnect to Matrix.

Without a `prefix-arg' ARG it will simply restart the
matrix-client-stream poller, but with a prefix it will disconnect and
connect, clearing all room data."
  (interactive "P")
  (if (or arg (not matrix-client-event-stream-end-token))
      (progn
        (matrix-client-disconnect)
        (matrix-client))
    (matrix-client-stream-from-end-token)))

(defun matrix-client-event-listener-callback (data)
  "The callback which `matrix-event-poll' pushes its data in to.

This calls each function in matrix-client-new-event-hook with the data
object with a single argument, DATA."
  (setq matrix-client-watchdog-last-message-ts
        (time-to-seconds))
  (unless (eq (car data) 'error)
    (dolist (hook matrix-client-new-event-hook)
      (funcall hook data)))
  (matrix-client-start-event-listener (matrix-get 'end data)))

(defun matrix-client-render-events-to-room (data)
  "Given a chunk of data from an /initialSyc, render each element from DATA in to its room."
  (let ((chunk (matrix-get 'chunk data)))
    (mapc 'matrix-client-render-event-to-room chunk)))

(defun matrix-client-restart-listener-maybe (sym error-thrown)
  "The error handler for matrix-client's event-poll.

SYM and ERROR-THROWN come from Request and are used to decide whether to connect."
  (cond ((or (string-match "code 6" (cdr error-thrown))
             (eq sym 'parse-error)
             (eq sym 'timeout)
             (string-match "interrupt" (cdr error-thrown))
             (string-match "code 7" (cdr error-thrown)))
         (message "Lost connection with matrix, will re-attempt in %s ms"
                  (/ matrix-client-event-poll-timeout 2))
         (matrix-client-restart-later))
        ((string-match "code 60" (cdr error-thrown))
         (message "curl couldn't validate CA, not advising --insecure? File bug pls."))))

(defun matrix-client-stream-from-end-token ()
  "Restart the matrix-client stream from the saved end-token."
  (matrix-client-start-event-listener matrix-client-event-stream-end-token))

(defun matrix-client-restart-later ()
  "Try to restart the Matrix poller later, maybe."
  (run-with-timer (/ matrix-client-event-poll-timeout 1000) nil
                  'matrix-client-stream-from-end-token))


(cl-defmethod matrix-send-async-with-url-retrieve-async
    ((con matrix-connection) method path
     &key content query api-version success error)
  "Perform an asynchronous Matrix API call.

METHOD is the HTTP method the given API endpoint PATH uses.
CONTENT is an optional `json-encode' compatible list which will
be used as the data in a POST or PUT request.  QUERY-PARAMS is an
optional alist of URL parameters.  HEADERS is optional HTTP
headers to add to the request.  CALLBACK is the callback which
will be called by `request' when the call completes"
  (declare (indent defun))
  (let* ((token (oref con token))
         (endpoint (concat (matrix-homeserver-api-url api-version) path))
         headers data)
    (when token
      (map-put query "access_token" token))
    (setq headers (map-put headers "Content-Type" "application/json")
          data (when content
                 (json-encode content)))
    (with-url-retrieve-async endpoint
      :method method
      :query query
      :extra-headers headers
      :data data
      :error error
      :success success)))



(cl-defmethod matrix-sync-with-url-retrieve-async ((con matrix-connection) &key since full-state timeout success error)
  (declare (indent defun))
  (matrix-send-async-with-url-retrieve-async
   con "GET" "/sync"
   :query (matrix--alist "timeout" (int-to-string timeout)
                         "full_state" (if full-state "true" "false")
                         "since" since)
   :api-version "r0"
   :success success
   :error error))

(let ((url-debug t))
  (with-current-buffer "Michael"
    (with-slots (con end-token) matrix-client-room-object
      (matrix-sync-with-url-retrieve-async con
                                           :timeout 30000
                                           :full-state t
                                           :since end-token
                                           :success (lambda (&rest args)
                                                      (message "\n\nSUCCESS: %s" args))
                                           :error (lambda (&rest args)
                                                    (message "\n\nERROR: %s" args))))))

(url-build-query-string (list (list 'access_token "TOKEN")) nil 'keep-empty)

(with-url-retrieve-async "https://example.com/sync"
  :method 'get
  :query (a-list "timeout" (int-to-string 30000)
                 "full_state" "true"
                 "since" "SINCE"
                 "access_token" "TOKEN")
  :extra-headers (a-list "Content-Type" "application/json")
  :data nil
  :error (message "\n\nERROR: %s" (list :buffer (current-buffer)
                                        :status status
                                        :cbargs cbargs
                                        :headers headers
                                        :body body
                                        :error errors))
  :success (message "\n\nSUCCESS: %s" (list :buffer (current-buffer)
                                            :status status
                                            :cbargs cbargs
                                            :headers headers
                                            :body body)))


(with-url-retrieve-async "https://matrix.org/_matrix/client/r0/sync"
  :inhibit-cookies t
  :method 'get
  :query (with-current-buffer "Michael"
           (with-slots* (((con) matrix-client-room-object)
                         ((token end-token) con))
             (a-list "timeout" (int-to-string 30000)
                     "full_state" "true"
                     "access_token" token
                     "since" end-token)))

  :error (lambda (&rest args)
           (message "\n\nERROR: %s" headers))
  :success (lambda (&rest ignore)
             (message "\n\nSUCCESS: %s" (json-read-from-string body))))

(with-url-retrieve-async "https://matrix.org/_matrix/client/r0/sync"
  :method 'get
  :query (with-current-buffer "Michael"
           (with-slots* (((con) matrix-client-room-object)
                         ((token end-token) con))
             (a-list "timeout" (int-to-string 30000)
                     "full_state" "true"
                     "access_token" token
                     "since" end-token)))
  :parse-body-fn #'json-read
  :error (lambda (&rest args)
           (message "\n\nERROR: %s" headers))
  :success (lambda (&rest ignore)
             (message "\n\nSUCCESS: %s" body)))

(cl-symbol-macrolet ((headers (buffer-substring (point) url-http-end-of-headers))
                     (body (buffer-substring (1+ url-http-end-of-headers) (point-max))))
  ;; Check for errors
  (lambda (&rest args)
    (message "\n\nERROR: %s" headers)))
(cl-symbol-macrolet ((headers (buffer-substring (point) url-http-end-of-headers))
                     (body (buffer-substring (1+ url-http-end-of-headers) (point-max))))
  ;; Check for errors
  (lambda (&rest args)
    (message "\n\nERROR: %s" (progn
                               (goto-char url-http-end-of-headers)
                               (json-read)))))

(with-url-retrieve-async "https://matrix.org/_matrix/client/r0/sync"
  :method 'get
  :query (with-current-buffer "room name"
           (with-slots* (((con) matrix-client-room-object)
                         ((token end-token) con))
             (a-list "timeout" (int-to-string 30000)
                     "full_state" "true"
                     "access_token" token
                     "since" end-token)))
  :parse-body-fn #'json-read
  :error (lambda (&rest args)
           (message "\n\nERROR: %s" headers))
  :success (lambda (&rest ignore)
             (message "\n\nSUCCESS: %s" body)))


;; ERROR: (:buffer  *http example.com:443* :status (:error (error http 404) :peer (:certificate (:version 3 :serial-number 0e:64:c5:fb:c2:36:ad:e1:4b:17:2a:eb:41:c7:8c:b0 :issuer C=US,O=DigiCert Inc,OU=www.digicert.com,CN=DigiCert SHA2 High Assurance Server CA :valid-from 2015-11-03 :valid-to 2018-11-28 ...) :key-exchange ECDHE-RSA :protocol TLS1.2 :cipher AES-128-GCM :mac AEAD)) :cbargs nil :headers HTTP/1.1 404 Not Found
;; Content-Encoding: gzip
;; Cache-Control: max-age=604800
;; Content-Type: text/html
;; Date: Fri, 01 Jun 2018 05:24:30 GMT
;; Expires: Fri, 08 Jun 2018 05:24:30 GMT
;; Server: EOS (vny006/0453)
;; Vary: Accept-Encoding
;; Content-Length: 606
;;  :body <!doctype html>
;; <html>
;; <head>
;;     <title>Example Domain</title>

(defvar url-response-body nil)

(cl-defun with-url-retrieve-async-fn (url &key cbargs silent inhibit-cookies data
                                          method extra-headers query success error
                                          parse-body-fn)
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
                            (otherwise `(lambda ()
                                          ,success))))
         (error-body-fn (cl-typecase error
                          (function error)
                          (otherwise `(lambda ()
                                        (let ((errors (plist-get status :error)))
                                          ,error)))))
         (url-request-data data)
         (url-request-method (upcase (cl-typecase method
                                       (symbol (symbol-name method))
                                       (string method))))
         (url-request-extra-headers extra-headers)
         (parse-body-fn parse-body-fn)
         (callback (lambda (status &optional cbargs)
                     (unwind-protect
                         ;; This is called by `url' with the current buffer already being the
                         ;; response buffer.

                         ;; FIXME: We can use `cl-symbol-macrolet' instead of `let' here,
                         ;; which only evaluates `headers' and `body' if they are present in
                         ;; the body-fns, which would be good, to avoid making strings when
                         ;; they are not used.  However, if the body-fns are passed to the
                         ;; macro as function symbols rather than body forms, the macrolet
                         ;; would not activate, because it can't see into the other functions'
                         ;; bodies (or if it could, that would probably be way too complicated
                         ;; and a bad idea).  So we might want to figure out a way to make the
                         ;; strings optional.  Or maybe we could set a parser arg, similar to
                         ;; `request', so that e.g. `json-read' could read the response buffer
                         ;; directly, instead of turning the response into a string, then
                         ;; using `json-read-from-string', which inserts back into a temp
                         ;; buffer, which is wasteful.  However, for the headers, I think it's
                         ;; reasonable to always bind them as a string, because the headers
                         ;; aren't very long, especially compared to a long HTML or JSON
                         ;; document.
                         (let ((headers (buffer-substring (point) url-http-end-of-headers))
                               (body (if parse-body-fn
                                         (progn
                                           (goto-char (1+ url-http-end-of-headers))
                                           (funcall parse-body-fn))
                                       (buffer-substring (1+ url-http-end-of-headers) (point-max)))))
                           ;; Check for errors
                           (pcase status
                             ;; NOTE: This may need to be updated to correctly handle multiple errors
                             (`(:error . ,_) (funcall error-body-fn
                                                      :cbargs cbargs
                                                      :status status
                                                      :error (plist-get status :error)
                                                      :headers headers
                                                      :body body))
                             ((or 'nil `(:peer (:certificate . ,_) . ,_))
                              (funcall success-body-fn
                                       :cbargs cbargs
                                       :status status
                                       :headers headers
                                       :body body))
                             (_ (error "Response status unrecognized; please report this error: %s" status))))
                       (unless (kill-buffer (current-buffer))
                         (warn "Unable to kill response buffer: %s" (current-buffer))))))
         url-obj filename query-string query-params)
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
    ;;  (message "\n\nDEBUG: %s" (list 'url-retrieve url callback ,cbargs ,silent ,inhibit-cookies))
    (url-retrieve url callback cbargs silent inhibit-cookies)))

(with-url-retrieve-async-fn "https://matrix.org/_matrix/client/r0/sync"
  :method 'get
  :query (with-current-buffer "Michael"
           (with-slots* (((con) matrix-client-room-object)
                         ((token end-token) con))
             (a-list "timeout" (int-to-string 30000)
                     "full_state" "true"
                     "access_token" token
                     "since" end-token)))
  :parse-body-fn #'json-read
  :error (cl-function
          (lambda (&key error &allow-other-keys)
            (message "\n\nERROR: %s" error)))
  :success #'test-success)

(cl-defun test-success (&key body &allow-other-keys)
  (message "\n\nSUCCESS: %s" body))


(with-url-retrieve-async2 "http://alphapapa.net/"
  :method 'get
  :query (a-list "timeout" (int-to-string 30000)
                 "full_state" "true"
                 "since" "SINCE"
                 "access_token" "TOKEN")
  :extra-headers (a-list "Content-Type" "application/json")
  :data nil
  :error (message "\n\nERROR: %s" (list :buffer (current-buffer)
                                        :status status
                                        :cbargs cbargs
                                        :headers headers
                                        :body body
                                        :error errors))
  :success (message "\n\nSUCCESS: %s" (list :buffer (current-buffer)
                                            :status status
                                            :cbargs cbargs
                                            :headers headers
                                            :body body)))

(cl-defun with-url-retrieve-async-fn2 (url &key cbargs silent inhibit-cookies data
                                           (method "GET") extra-headers query success error
                                           parser)
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
                                          (lambda (&key cbargs status headers body)
                                            ,success))))))
         (error-body-fn (cl-typecase error
                          (function error)
                          (otherwise (byte-compile
                                      `(cl-function
                                        (lambda (&key cbargs status error headers body)
                                          ,error))))))
         (url-request-data data)
         (url-request-method (upcase (cl-typecase method
                                       (symbol (symbol-name method))
                                       (string method))))
         (url-request-extra-headers extra-headers)
         (callback (lambda (status &optional cbargs)
                     (unwind-protect
                         ;; This is called by `url' with the current buffer already being the
                         ;; response buffer.

                         ;; FIXME: We can use `cl-symbol-macrolet' instead of `let' here,
                         ;; which only evaluates `headers' and `body' if they are present in
                         ;; the body-fns, which would be good, to avoid making strings when
                         ;; they are not used.  However, if the body-fns are passed to the
                         ;; macro as function symbols rather than body forms, the macrolet
                         ;; would not activate, because it can't see into the other functions'
                         ;; bodies (or if it could, that would probably be way too complicated
                         ;; and a bad idea).  So we might want to figure out a way to make the
                         ;; strings optional.  Or maybe we could set a parser arg, similar to
                         ;; `request', so that e.g. `json-read' could read the response buffer
                         ;; directly, instead of turning the response into a string, then
                         ;; using `json-read-from-string', which inserts back into a temp
                         ;; buffer, which is wasteful.  However, for the headers, I think it's
                         ;; reasonable to always bind them as a string, because the headers
                         ;; aren't very long, especially compared to a long HTML or JSON
                         ;; document.
                         (let ((headers (buffer-substring (point) url-http-end-of-headers))
                               (body (if parser
                                         (progn
                                           (goto-char (1+ url-http-end-of-headers))
                                           (funcall parser))
                                       (buffer-substring (1+ url-http-end-of-headers) (point-max)))))
                           ;; Check for errors
                           (pcase status
                             ;; NOTE: This may need to be updated to correctly handle multiple errors
                             (`(:error . ,_) (funcall error-body-fn
                                                      :cbargs cbargs
                                                      :status status
                                                      :error (plist-get status :error)
                                                      :headers headers
                                                      :body body))
                             ((or 'nil `(:peer (:certificate . ,_) . ,_))
                              (funcall success-body-fn
                                       :cbargs cbargs
                                       :status status
                                       :headers headers
                                       :body body))
                             (_ (error "Response status unrecognized; please report this error: %s" status))))
                       (unless (kill-buffer (current-buffer))
                         (warn "Unable to kill response buffer: %s" (current-buffer))))))
         url-obj filename query-string query-params)
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
    ;;  (message "\n\nDEBUG: %s" (list 'url-retrieve url callback ,cbargs ,silent ,inhibit-cookies))
    (url-retrieve url callback cbargs silent inhibit-cookies)))

(with-url-retrieve-async-fn2 "http://alphapapa.net/"
  :method 'get
  :query (a-list "timeout" (int-to-string 30000)
                 "full_state" "true"
                 "since" "SINCE"
                 "access_token" "TOKEN")
  :extra-headers (a-list "Content-Type" "application/json")
  :data nil
  :error '(message "\n\nERROR: %s" (list :buffer (current-buffer)
                                         :status status
                                         :cbargs cbargs
                                         :headers headers
                                         :body body
                                         :error error))
  :success '(message "\n\nSUCCESS: %s" (list :buffer (current-buffer)
                                             :status status
                                             :cbargs cbargs
                                             :headers headers
                                             :body body)))

;; NOTE: This is working.  Seems to make more sense to use a function rather than a macro, because
;; the only benefit of using a macro is not having to quote non-function :success and :error body
;; forms.

(url-with-retrieve-async "https://example.com/404"
  :error '(message "\n\nERROR: %s" error)
  :parser (lambda ()
            (libxml-parse-html-region (point) (point-max)))
  :success #'test-success)

;; NOTE: See current version of function at https://github.com/alphapapa/elexandria/blob/master/elexandria.el#L187
