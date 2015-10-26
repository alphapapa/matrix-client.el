;;; mclient-modes.el --- Modes for a Matrix.org chat client

;; Copyright (C) 2015 Ryan Rix
;; Author: Ryan Rix <ryan@whatthefuck.computer>
;; Maintainer: Ryan Rix <ryan@whatthefuck.computer>
;; Created: 21 June 2015
;; Keywords: web
;; Homepage: http://doc.rix.si/matrix.html
;; Package-Version: 0.1.0
;; Package-Requires: ((json))

;; This file is not part of GNU Emacs.

;; mclient-modes.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.
;;
;; mclient-modes.el is distributed in the hope that it will be useful, but
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

(defvar matrix-client-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'mclient-send-active-line)
    map)
  "Keymap for `matrix-client-mode'.")

(define-derived-mode matrix-client-mode fundamental-mode "Matrix Client"
  "Major mode for Matrix client buffers.

\\{matrix-client-mode-map}")

(defface mclient-metadata
  '((((class color) (background light)) (:foreground "#000088" :weight bold))
    (((class color) (background dark)) (:foreground "#4444FF" :weight bold))
    (t (:weight bold)))
  "Face for chat metadata properties."
  :group 'mclient-faces)

(provide 'mclient-modes)
;;; mclient-modes.el ends here
