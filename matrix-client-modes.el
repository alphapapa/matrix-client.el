;;; matrix-client-modes.el --- Modes for a Matrix.org chat client

;; Copyright (C) 2015 Ryan Rix
;; Author: Ryan Rix <ryan@whatthefuck.computer>
;; Maintainer: Ryan Rix <ryan@whatthefuck.computer>
;; Created: 21 June 2015
;; Keywords: web
;; Homepage: http://doc.rix.si/matrix.html
;; Package-Version: 0.1.0
;; Package-Requires: ((json))

;; This file is not part of GNU Emacs.

;; matrix-client-modes.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.
;;
;; matrix-client-modes.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; this file describes the major-mode for Matrix-Client buffers. See the docstring
;; for `matrix-client-mode' for more information.

;;; Code:

(require 'simple)

(require 'dash)

(defvar matrix-client-mode-map
  (let ((map (make-sparse-keymap))
        (mappings '(
                    "r" matrix-client-reply-or-insert
                    "R" (lambda () (interactive) (matrix-client-reply-or-insert t))
                    "RET" matrix-client-send-active-line
                    "DEL "matrix-client-delete-backward-char
                    "M-v" matrix-client-scroll-down
                    )))
    (cl-loop for (key fn) on mappings by #'cddr
             do (define-key map (kbd key) fn))
    map)
  "Keymap for `matrix-client-mode'.")

(defun matrix-client-reply-or-insert (&optional quote-p)
  "If point is on a previous message, begin a reply addressed to its sender.  Otherwise, self-insert.
With prefix, quote message or selected region of message."
  (interactive "P")
  (if (get-text-property (point) 'sender)
      ;; Start reply
      (let* ((sender (get-text-property (point) 'sender))
             (display-name (get-text-property (point) 'display-name))
             (quote (if quote-p
                        ;; FIXME: Also quote in HTML format
                        (--> (if (use-region-p)
                                 (buffer-substring (region-beginning) (region-end))
                               (matrix-client--this-message))
                             (replace-regexp-in-string (rx bol) "> " it)
                             (concat it "\n\n")
                             (progn
                               (remove-text-properties 0 (length it) '(read-only t) it)
                               it))
                      ;; Not quoting
                      "")))
        ;; FIXME: Insert a link to username, and use a filter to transform to HTML before sending.
        (goto-char (matrix-client--prompt-position))
        (insert display-name ": " quote))
    ;; Do self-insert
    (setq this-command 'self-insert-command)
    (call-interactively 'self-insert-command)))

(defun matrix-client--this-message ()
  "Return message point is on."
  (let* ((beg (previous-single-property-change (point) 'event_id))
         (end (next-single-property-change (point) 'event_id))
         ;; Skip past metadata
         (message-beg (next-single-property-change beg 'face)))
    (buffer-substring message-beg end)))

(define-derived-mode matrix-client-mode fundamental-mode "Matrix Client"
  "Major mode for Matrix client buffers.

\\{matrix-client-mode-map}")

(defface matrix-client-link
  '((t (:inherit link)))
  "Face for links in messages."
  :group 'matrix-client)

(defface matrix-client-metadata
  '((((class color) (background light)) (:foreground "#000088" :weight bold))
    (((class color) (background dark)) (:foreground "#4444FF" :weight bold))
    (t (:weight bold)))
  "Face for chat metadata properties."
  :group 'matrix-client)

(defface matrix-client-own-metadata
  '((((class color) (background light)) (:foreground "#268bd2" :weight bold))
    (((class color) (background dark)) (:foreground "#268bd2" :weight bold))
    (t (:weight bold)))
  "Face for user's own chat metadata properties."
  :group 'matrix-client)

(defface matrix-client-own-messages
  '((((class color) (background light)) (:foreground "#586e75" :weight bold :slant italic))
    (((class color) (background dark)) (:foreground "#586e75" :weight bold :slant italic))
    (t (:weight bold :slant italic)))
  "Face for user's own chat messages."
  :group 'matrix-client)

(defface matrix-client-notice
  '((t (:inherit font-lock-comment-face)))
  "Face for notices."
  :group 'matrix-client)

(defface matrix-client-notice-metadata
  '((t (:inherit font-lock-comment-face)))
  "Face for notices."
  :group 'matrix-client)

(defface matrix-client-last-seen
  '((t (:inherit highlight :height 0.1)))
  "Face for last-seen overlay."
  :group 'matrix-client)

(defface matrix-client-date-header
  '((t (:inherit highlight :weight bold)))
  "Face for date headers."
  :group 'matrix-client)

(provide 'matrix-client-modes)
;;; matrix-client-modes.el ends here
