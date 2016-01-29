;;; matrix-sauron.el --- Sauron integration for the Emacs Matrix Client

;; Copyright (C) 2015 Ryan Rix
;; Author: Ryan Rix <ryan@whatthefuck.computer>
;; Maintainer: Ryan Rix <ryan@whatthefuck.computer>
;; Created: 21 June 2015
;; Keywords: web
;; Homepage: http://doc.rix.si/matrix.html
;; Package-Version: 0.1.0

;; This file is not part of GNU Emacs.

;; matrix-sauron.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.
;;
;; matrix-sauron.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library pushes Matrix events in to Sauron, allowing you to easily see a
;; list of notifications and jump to them. See https://github.com/djcb/sauron
;; for information about Sauron.

;; To use this, install Sauron via package.el or manually, and then load or eval
;; this library. Add 'sauron-matrix to sauron-modules and run [`sauron-start']

;;; Code:

(require 'sauron nil 'noerror)

(defvar sauron-prio-matrix-new-messages 2
  "Priority for incoming Matrix events.")

(defvar sauron-matrix-running nil
  "When non-nil, matrix-sauron is running.")

(defvar matrix-sauron-pushrules nil
  "Matrix homeserver push rules")

(defun matrix-sauron-start ()
  "Start matrix-sauron."
  (if (and (boundp 'matrix-homeserver-base-url)
           matrix-homeserver-base-url)
      (progn
        (when sauron-matrix-running
          (error "matrix-sauron is already running. Call sauron-matrix-stop first."))
        (matrix-sauron-fetch-rules)
        (add-hook 'matrix-client-new-event-hook 'matrix-add-sauron-event)
        (setq sauron-matrix-running t))
    (message "matrix-client not loadable, so matrix-sauron could not start.")))

(defun matrix-sauron-stop ()
  "Stops and cleans up matrix-sauron."
  (when sauron-matrix-running
    (remove-hook 'matrix-client-new-event-hook 'matrix-add-sauron-event)
    (setq sauron-matrix-running nil)))

(defun matrix-sauron-fetch-rules ()
  "Fetch and parse the push rules from the server."
  (let* ((resp (matrix-send "GET" "/pushrules/")))
    (setq matrix-sauron-pushrules (matrix-get 'global resp))))

(defun matrix-sfind (str array)
  (condition-case nil
      (cl-find str array :test 'string-equal)
    (error nil)))

(defun matrix-sauron-process-event-with-pushrules (prio room-id membership content username)
  ;; Process each pushrule type-break
  (let ((final-prio
         (+ prio
            ;; Underride
            ;; Rooms
            (reduce
             #'+
             (map 'list
                  (lambda (rule)
                    (when (matrix-get 'enabled rule)
                      (let* ((actions (matrix-get 'actions rule))
                             (rule-id (matrix-get 'rule_id rule))
                             (score-mod (if (matrix-sfind "dont_notify" actions)
                                            -5 1)))
                        (if (string-equal room-id rule-id)
                            (progn
                              (message "%d %d %s" -prio score-mod rule-id)
                              score-mod)
                          0))))
                  (matrix-get 'room matrix-sauron-pushrules)))
            ;; Content
            (reduce
             #'+
             (map 'list
                  (lambda (rule) 
                    (when (matrix-get 'enabled rule)
                      (let* ((actions (matrix-get 'actions rule))
                             (rule-id (matrix-get 'rule_id rule))
                             (body (matrix-get 'body content))
                             (pattern (matrix-get 'pattern rule))
                             (score-mod (if (matrix-sfind "dont_notify" actions)
                                            -5 1)))
                        (if (and body (string-match pattern body))
                            (progn
                              (message "%d %d %s" -prio score-mod rule-id)
                              score-mod)
                          0))))
                  (matrix-get 'content matrix-sauron-pushrules)))
            ;; Sender
            ;; Override
            )))
    (cond ((< 0 final-prio) 0)
          ((< final-prio 5) 5)
          (t final-prio))))

(defun matrix-add-sauron-event (chunk)
  (mapc (lambda (data)
          (let* ((room-id (matrix-get 'room_id data))
                 (room-buf (matrix-get room-id matrix-client-active-rooms))
                 (membership (if room-buf
                                 (with-current-buffer room-buf
                                   (length matrix-client-room-membership))
                               0))
                 (type (matrix-get 'type data))
                 (content (matrix-get 'content data))
                 (username (matrix-get 'user_id data))
                 (prio sauron-prio-matrix-new-messages)
                 (prio (matrix-sauron-process-event-with-pushrules prio room-id membership content username))
                 (target (if (buffer-live-p room-buf)
                             (save-excursion
                               (with-current-buffer room-buf
                                 (end-of-buffer)
                                 (previous-line)
                                 (point-marker))))))
            (when (not (and username (string-match matrix-username username)))
              (sauron-add-event 'matrix prio
                                (format "<%s> %s" username (matrix-get 'body content))
                                (lexical-let* ((target-mark target)
                                               (target-buf room-buf))
                                  (lambda ()
                                    (sauron-switch-to-marker-or-buffer (or target-mark target-buf))))))))
        (matrix-get 'chunk chunk)))

(provide 'matrix-sauron)

;; End of matrix-sauron.el
