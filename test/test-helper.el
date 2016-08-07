;;; test-helper --- Test helper for matrix-client

;;; Commentary:
;; test helper inspired from https://github.com/tonini/overseer.el/blob/master/test/test-helper.el

;;; Code:

(require 'f)

(defvar cpt-path
  (f-parent (f-this-file)))

(defvar matrix-client-test-path
  (f-dirname (f-this-file)))

(defvar matrix-client-root-path
  (f-parent matrix-client-test-path))

(defvar matrix-client-sandbox-path
  (f-expand "sandbox" matrix-client-test-path))

(when (f-exists? matrix-client-sandbox-path)
  (error "Something is already in %s. Check and destroy it yourself" matrix-client-sandbox-path))

(defmacro within-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory matrix-client-sandbox-path))
     (when (f-exists? matrix-client-sandbox-path)
       (f-delete default-directory :force))
     (f-mkdir matrix-client-sandbox-path)
     ,@body
     (f-delete default-directory :force)))

(require 'ert)
(require 'el-mock)
(eval-when-compile
    (require 'cl))
(require 'matrix-client)

(provide 'test-helper)
;;; test-helper.el ends here
