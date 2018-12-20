#!/bin/bash
# -*- mode: emacs-lisp; -*-

#;; This file can be executed directly as a "standalone" Emacs Matrix
#;; client that does not load any user configuration files.  The Bash
#;; process substitution trick above was inspired by:
#;; https://superuser.com/a/821624

while [[ $1 ]]
do
[[ $1 == --upgrade ]] && upgrade=" (setq upgrade-matrix-client t) (setq quelpa-update-melpa-p t)"
[[ $1 == --debug ]] && debug="(setq debug-on-error t)"
shift
done

emacs -q --insert <(tail -n +24 $0) --eval="(progn
(defvar upgrade-matrix-client nil)
(defvar quelpa-update-melpa-p nil)
$upgrade $debug
(eval-buffer))"

exit

;;; matrix-client-standalone.el

;; NOTE: If you move the elisp header above, you must also change the line number passed to tail.
;; TODO: To be super slick, we could get the line number with grep.

(setq tool-bar-mode nil)

;;; package.el

(require 'package)
(setq package-menu-async nil)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;;; Quelpa

;; Install Quelpa if necessary.
(unless (require 'quelpa-use-package nil t)
  (defun install-quelpa ()
    (package-install 'quelpa-use-package)
    (require 'quelpa-use-package nil t))
  (add-hook 'package--post-download-archives-hook #'install-quelpa)
  (package-list-packages))

;; Install/upgrade Matrix Client. Quelpa makes it very difficult to force an upgrade.  It says if
;; you add ":upgrade t", it will, but it doesn't work.  So we have to bind `current-prefix-arg'.
(let ((current-prefix-arg upgrade-matrix-client))
  (quelpa '(matrix-client :fetcher github :repo "jgkamat/matrix-client-el"
	                  :files (:defaults "logo.png" "misc/matrix-client-standalone.el"))))

;;; matrix-client

(use-package matrix-client
  :custom
  (matrix-client-save-token t)
  (matrix-client-show-images t)
  (matrix-log t)
  (matrix-client-show-room-avatars t)
  (matrix-client-mark-modified-rooms t))

;;;; Connect

(add-hook 'matrix-client-setup-room-buffer-hook
          (lambda (&rest _ignore)
            (setq mode-line-format nil)))

(with-selected-frame
    (call-interactively #'matrix-client-frame)
  (delete-other-frames))
