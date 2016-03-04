;;; matrix-helpers.el --- Helpers for matrix-client

;; Copyright (C) 2015-2016 Ryan Rix
;; Author: Ryan Rix <ryan@whatthefuck.computer>
;; Maintainer: Ryan Rix <ryan@whatthefuck.computer>
;; Created: 4 March 2016
;; Keywords: web
;; Homepage: http://doc.rix.si/matrix.html
;; Package-Version: 0.3.0

;; This file is not part of GNU Emacs.

;; matrix-helpers.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option) any
;; later version.
;;
;; matrix-helpers.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Helpers for [`matrix-api'].

;;; Code:

(defun matrix-homeserver-api-url (&optional version)
  "Message `matrix-homeserver-base-url' in to a fully-qualified API endpoint URL."
  (let ((version (or version "v1")))
    (format "%s/_matrix/client/api/%s" matrix-homeserver-base-url version)))

(defun matrix-get (key obj)
  "Easy JSON accessor, get KEY's value from OBJ."
  (cdr (assoc key obj)))

(defun matrix-transform-mxc-uri (uri)
  "Turn an MXC content URI in to an HTTP URL."
  (let ((components (split-string uri "/")))
    (format "%s/_matrix/media/v1/download/%s/%s"
            matrix-homeserver-base-url
            (elt components 2)
            (elt components 3))))

(provide 'matrix-helpers)
;;; matrix-helpers.el ends here
