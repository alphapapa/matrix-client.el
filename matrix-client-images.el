(require 'shr)

(require 'request)
(require 's)

(defcustom matrix-client-show-images nil
  "Download and show images posted to rooms."
  :type 'boolean
  :group 'matrix-client)

(defcustom matrix-client-image-url-prefixes
  (list (rx bow "http" (optional "s") "://"
            (or "i.imgur.com" "i.redd.it")
            "/"))
  "List of regexps matching parts of URLs to images that should be downloaded and displayed.
Each regexp should match from the beginning of the URL, including
the protocol type, if desired.  It will automatically be extended
to match until the next whitespace character."
  :type '(repeat string)
  :group 'matrix-client)

(defun matrix-client--image-urls (text)
  "Return list of supported image URLs in TEXT."
  (cl-loop for regexp in matrix-client-image-url-prefixes
           for regexp = (rx-to-string `(seq (regexp ,regexp) (1+ (not space))))
           append (-map #'first (s-match-strings-all regexp text))))

(cl-defmethod matrix-client-insert-image ((room matrix-client-room) message-id url)
  "Download image from URL and insert it at message MESSAGE-ID in ROOM."
  (url-with-retrieve-async url
    :silent t
    :inhibit-cookies t
    :query-on-exit nil
    :parser (apply-partially #'matrix-client-parse-image room)
    :success (apply-partially #'matrix-client-insert-image-callback
                              :room room
                              :message-id message-id
                              :url url)))

(cl-defmethod matrix-client-parse-image ((room matrix-client-room) &rest rescale-args)
  "Parse image from current HTTP response buffer and return image object.
RESCALE-ARGS are passed to `matrix-client-rescale-image'."
  (pcase-let* ((data (progn
                       ;; Disabling multibyte is required for reading binary data.
                       ;; FIXME: require or autoload this function
                       (mm-disable-multibyte)
                       ;; Point is where the body starts, after the headers
                       (buffer-substring (point) (point-max))))
               ((eieio buffer) room))
    (with-current-buffer buffer
      ;; Rescale image in room buffer to get proper size
      (apply #'matrix-client-rescale-image data rescale-args))))

(cl-defun matrix-client-rescale-image (data &key max-width max-height &allow-other-keys)
  "Rescale DATA, if too big, to fit the current buffer.
MAX-WIDTH and MAX-HEIGHT are used if set, otherwise they are
determined by the size of the buffer's window."
  ;; Based on image.el
  (when (fboundp 'imagemagick-types)
    (cond ((and max-width max-height)
           ;; Use given size
           )
          ((get-buffer-window (current-buffer))
           ;; Use window size
           (setq max-width (or max-width (window-pixel-width))
                 max-height (or max-height (/ (window-pixel-height) 2))))
          ((current-buffer)
           ;; Buffer not displayed; use frame
           (setq max-width (or max-width (frame-pixel-width))
                 max-height (or max-height (/ (frame-pixel-height) 2))))
          (t
           ;; This should not happen with the fixes above, but just in case:
           (warn "Weird error rescaling image, please report.  Buffer: %s" (current-buffer))))
    (create-image data 'imagemagick 'data-p
                  :max-width max-width
                  :max-height max-height)))

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
