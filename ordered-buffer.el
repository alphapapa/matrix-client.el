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

;; The result of factoring out some of the buffer-insertion code in matrix-client-room.el.  Could be
;; useful in other projects.  The basic idea is to insert strings into a buffer, ordered by a text
;; property (like an integer timestamp).  The strings may "arrive" in any order, but they will
;; always be inserted at the correct, ordered position.

;;; Code:

(require 'cl-lib)

(defface ordered-buffer-header
  '((t (:inherit highlight :weight bold)))
  "Face for headers."
  :group 'ordered-buffer)

(defvar ordered-buffer-header-face 'ordered-buffer-header
  "Face applied to headers.")
(defvar ordered-buffer-point-fn #'point-max
  "Function called by `ordered-buffer-insert' which returns the position at which to insert a new string.")
(defvar ordered-buffer-prefix-fn nil
  "An optional function which is called by `ordered-buffer-insert' before inserting its string.
Called with point at the insertion position.  May be used to
insert headers, etc.")
(defvar ordered-buffer-header-suffix "\n"
  "String inserted after headers.  May be nil.")

(defun ordered-buffer-insert (string &rest properties)
  "Insert STRING into current buffer at appropriate position.
The `ordered-buffer-point-fn' function returns the position.  If
`ordered-buffer-prefix-fn' is non-nil, it is called with point at
the position before inserting STRING.  PROPERTIES are applied to
STRING."
  (save-excursion
    (goto-char (funcall ordered-buffer-point-fn))
    (when ordered-buffer-prefix-fn
      (funcall ordered-buffer-prefix-fn))
    (insert (apply #'propertize string properties))))

(defun ordered-buffer-insert-header (string &rest properties)
  "Insert header containing STRING at point.
PROPERTIES are applied to STRING, and the face in
`ordered-buffer-header-face' is applied to it.  The string
`ordered-buffer-header-suffix' is appended to the header.  The
header has the text-property `ordered-buffer-header' set."
  (let* ((visible-header (propertize (concat " " string "\n")
                                     'face ordered-buffer-header-face))
         (whole-header (apply #'propertize (concat "\n" visible-header ordered-buffer-header-suffix)
                              'ordered-buffer-header t
                              'read-only t
                              properties)))
    (insert whole-header)))

(cl-defun ordered-buffer-point-fn (&key backward-from forward-from property comparator value
                                        final-fn)
  "Return position at which a new string should be inserted, depending on criteria.

One of BACKWARD-FROM or FORWARD-FROM may be set and should be an
integer position in the buffer or a function which returns a
position, from which the search starts.

PROPERTY should be a symbol of the text property (which should
not be a keyword symbol) which is compared with VALUE using
COMPARATOR.

When the comparison is non-nil, the point at that position is
returned.  If the search reaches a point after which PROPERTY
does not change again in the buffer, the point returned depends
on the search direction: if BACKWARD-FROM, `point-min'; if
FORWARD-FROM, `point-max'.

FINAL-FN may be a function which is run after finding the
position but before returning point.  It may move point to make a
final adjustment."
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
             until (when this-value
                     (funcall comparator this-value value))
             for next-pos = (funcall property-change-fn (point) property)
             if next-pos
             do (goto-char next-pos)
             else
             return (if backward-from
                        (point-min)
                      (point-max))
             finally do (when final-fn
                          (funcall final-fn))
             finally return (point))))


;;;; Footer

(provide 'ordered-buffer)

;;; ordered-buffer.el ends here
