(require 'matrix-api-r0.3.0)

(require 'buttercup)

(describe "matrix-client"

  (before-all
    (setq session (matrix-session :user "@matrix-client-el-tester:matrix.org"
                                  :device-id (md5 (concat "matrix-client.el/test.el" (current-time-string) (system-name)))
                                  :initial-device-display-name (format "matrix-client.el/test.el @ %s (%s)"
                                                                       (system-name) (format-time-string "%F %T"))))
    (setq password "testing")
    (setq matrix-synchronous t))

  (describe "login"
    (before-all
      (spy-on 'matrix-sync))
    (it "Can log in and get an access_token"
      (expect (progn
                (matrix-login session password)
                (oref session access-token))
              :to-match (rx (1+ alnum)))
      (expect 'matrix-sync :to-have-been-called))))
