;;;;; Faces

(defface matrix-client-metadata
  '((t (:weight bold :inherit font-lock-function-name-face)))
  "Face for chat metadata properties."
  :group 'matrix-client)

(defface matrix-client-own-metadata
  '((t (:weight bold :inherit font-lock-keyword-face)))
  "Face for user's own chat metadata properties."
  :group 'matrix-client)

(defface matrix-client-message-body
  '((t (:inherit default)))
  "Face for Matrix message bodies."
  :group 'matrix-client)

(defface matrix-client-own-message-body
  '((t (:inherit matrix-client-message-body)))
  "Face for user's own Matrix message bodies."
  :group 'matrix-client)

(defface matrix-client-quoted-message
  '((t (:inherit matrix-client-message-body :slant italic)))
  "Face for Matrix quoted messages."
  :group 'matrix-client)

(defface matrix-client-pending-messages
  '((((class color) (background light)) (:foreground "#586e75" :weight bold :slant italic))
    (((class color) (background dark)) (:foreground "#586e75" :weight bold :slant italic))
    (t (:weight bold :slant italic)))
  "Face for user's pending chat messages."
  :group 'matrix-client)

(defface matrix-client-failed-messages
  '((((class color) (background light)) (:foreground "red" :weight bold :slant italic))
    (((class color) (background dark)) (:foreground "red" :weight bold :slant italic))
    (t (:weight bold :slant italic)))
  "Face for user's failed chat messages."
  :group 'matrix-client)

(defface matrix-client-notice
  '((t :inherit font-lock-comment-face))
  "Face for notices."
  :group 'matrix-client)

(defface matrix-client-notice-metadata
  '((t :inherit font-lock-comment-face))
  "Face for notices."
  :group 'matrix-client)

(defface matrix-client-last-seen
  `((t (:background ,(face-attribute 'matrix-client-own-metadata :foreground nil t)
                    :height 0.1)))
  "Face for last-seen overlay."
  :group 'matrix-client)

(defface matrix-client-date-header
  '((t (:inherit highlight :weight bold)))
  "Face for date headers."
  :group 'matrix-client)

(provide 'matrix-client-faces)
