(require 'shr)

(require 'request)
(require 's)

(defcustom matrix-client-show-images nil
  "Download and show images posted to rooms."
  :type 'boolean)

(defcustom matrix-client-image-url-prefixes
  (list (rx bow "http" (optional "s") "://"
            (or "i.imgur.com" "i.redd.it")
            "/"))
  "List of regexps matching parts of URLs to images that should be downloaded and displayed.
Each regexp should match from the beginning of the URL, including
the protocol type, if desired.  It will automatically be extended
to match until the next whitespace character."
  :type '(repeat string))

(defun matrix-client--image-urls (text)
  "Return list of supported image URLs in TEXT."
  (cl-loop for regexp in matrix-client-image-url-prefixes
           for regexp = (rx-to-string `(seq (regexp ,regexp) (1+ (not space))))
           append (-map #'first (s-match-strings-all regexp text))))

(cl-defmethod matrix-client-insert-image ((room matrix-client-room) message-id url)
  "Download image from URL and insert it at message MESSAGE-ID in ROOM."
  (request url
           :parser (apply-partially #'matrix-client-parse-image room)
           :success (apply-partially #'matrix-client-insert-image-callback
                                     :room room
                                     :message-id message-id
                                     :url url)))

(cl-defmethod matrix-client-parse-image ((room matrix-client-room))
  "Parse image from current HTTP response buffer and return image object."
  (pcase-let* ((data (progn
                       ;; Disabling multibyte is required for reading binary data.
                       ;; FIXME: require or autoload this function
                       (mm-disable-multibyte)
                       ;; Point is where the body starts, after the headers
                       (buffer-substring (point) (point-max))))
               ((eieio buffer) room))
    (with-current-buffer buffer
      ;; Rescale image in room buffer to get proper size
      (matrix-client-rescale-image data))))

(defun matrix-client-rescale-image (data)
  "Rescale DATA, if too big, to fit the current buffer."
  ;; Copied from image.el
  (if (not (and (fboundp 'imagemagick-types)
                (get-buffer-window (current-buffer))))
      (create-image data nil t :ascent 100)
    (let ((edges (window-inside-pixel-edges
                  (get-buffer-window (current-buffer))))
          (max-width (window-pixel-width))
          (max-height (/ (window-pixel-height) 2)))
      (create-image data 'imagemagick 'data-p
                    :max-width max-width
                    :max-height max-height))))

(cl-defmethod matrix-client-insert-image-callback (&key (room matrix-client-room) message-id url
                                                        data error-thrown symbol-status response
                                                        &allow-other-keys)
  "Insert image into proper place at URL in message MESSAGE-ID in ROOM.
Image is passed from parser as DATA, which should be an image
object made with `create-image'.  This function should be called
as an async callback when the image is downloaded."
  (with-slots (buffer) room
    (with-current-buffer buffer
      ;; Starting with last message, search backward to find message
      (cl-loop initially do (goto-char (point-max))
               for event_id = (get-text-property (point) 'event_id)
               until (equal event_id message-id)
               do (goto-char (previous-single-property-change (point) 'event_id)))
      ;; Find beginning and end of message text
      (let* ((beg (point))
             (end (next-single-property-change beg 'event_id))
             (inhibit-read-only t)
             (props (text-properties-at beg))
             (string (with-temp-buffer
                       ;; NOTE: These space characters before the newlines
                       ;; are REQUIRED for some reason.  Without them,
                       ;; images disappear seemingly at random when
                       ;; the user scrolls, moves point, or types.
                       (insert " \n")
                       (insert-image data)
                       (insert " \n")
                       (apply #'propertize (buffer-string) props))))
        ;; Find URL and insert image after it
        (goto-char beg)
        (re-search-forward (regexp-quote url) end)
        (goto-char (match-end 0))
        (insert string)
        (forward-char)))))

(provide 'matrix-client-images)
