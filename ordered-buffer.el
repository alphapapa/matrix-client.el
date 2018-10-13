;;; ordered-buffer.el --- Insert strings into buffers in flexibly defined order  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: buffers

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defface ordered-buffer-header
  '((t (:inherit highlight :weight bold)))
  "Face for headers."
  :group 'ordered-buffer)

(defvar ordered-buffer-header-face 'ordered-buffer-header)
(defvar ordered-buffer-point-fn #'point-max)
(defvar ordered-buffer-prefix-fn nil)
(defvar ordered-buffer-header-suffix "\n"
  "String inserted after headers.  May be nil.")

(defun ordered-buffer-insert (string &rest properties)
  "FIXME."
  (save-excursion
    (goto-char (funcall ordered-buffer-point-fn))
    (when ordered-buffer-prefix-fn
      (funcall ordered-buffer-prefix-fn))
    (insert (apply #'propertize string properties))))

(defun ordered-buffer-insert-header (string &rest properties)
  "FIXME"
  (let* ((visible-header (propertize (concat " " string "\n")
                                     'face ordered-buffer-header-face))
         (whole-header (apply #'propertize (concat "\n" visible-header ordered-buffer-header-suffix)
                              'ordered-buffer-header t
                              'read-only t
                              properties)))
    (insert (apply #'propertize whole-header properties))))

(cl-defun ordered-buffer-point-fn (&key backward-from forward-from property comparator value)
  "FIXME"
  (declare (indent defun))
  (when (and backward-from forward-from)
    (user-error "Only one of `:backward-from' or `:forward-from' may be set"))
  (let* ((get-property-fn (cond (backward-from `(lambda ()
                                                  (get-text-property (if (> (point) 1)
                                                                         (1- (point))
                                                                       (point))
                                                                     ',property)))
                                (forward-from `(lambda ()
                                                 (get-text-property (point) ',property)))))
         (property-change-fn (cond (backward-from #'previous-single-property-change)
                                   (forward-from #'next-single-property-change)))
         (from (or backward-from forward-from)))
    (goto-char (cl-etypecase from
                 (function (funcall from))
                 (integer from)))
    (cl-loop for this-value = (funcall get-property-fn)
             until (or (not this-value)
                       (funcall comparator this-value value))
             for next-pos = (funcall property-change-fn (point) property)
             if next-pos
             do (goto-char next-pos)
             else
             return (if backward-from
                        (point-min)
                      (point-max))
             finally return (point))))


;;;; Footer

(provide 'ordered-buffer)

;;; ordered-buffer.el ends here
