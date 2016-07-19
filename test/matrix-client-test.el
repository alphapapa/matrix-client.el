;;; Test for `matrix-client'

;;; Commentary:
;; These are the tests for `matrix-client'

;;; Code:

(require 'matrix-helpers)

(ert-deftest matrix-parse-curl ()
  (should
   (eq 51
       (matrix-parse-curl-exit-code "exited abnormally with code 51")))
  (should
   (eq 60
       (matrix-parse-curl-exit-code "exited abnormally with code 60")))
  (should
   (eq nil
       (matrix-parse-curl-exit-code "bagels and beans"))))
