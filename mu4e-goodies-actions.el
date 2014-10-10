(require 'mu4e)

;; Actions
;;--------------------------------------------------

;; show the thread of current message
(defun mu4e-headerv-action-show-cur-thread (msg)
  "Only view the thread of current mail"
  (let ((msgid (mu4e-msg-field msg :message-id)))
    (setq
     mu4e-headers-show-threads t
     mu4e-headers-include-related t)
    (mu4e-headers-search (format "msgid:%s" msgid))))

(add-to-list 'mu4e-headers-actions
             '("open thread of the message at point" . mu4e-headerv-action-show-cur-thread) t)
(add-to-list 'mu4e-view-actions
             '("open thread of the message at point" . mu4e-headerv-action-show-cur-thread) t)

;; show current message's html part in browser
(defun mu4e-msgv-action-view-in-browser (msg)
  "View the body of the message in a web browser."
  (interactive)
  (let ((html (mu4e-msg-field (mu4e-message-at-point t) :body-html))
        (tmpfile (format "%s/%d.html" temporary-file-directory (random))))
    (unless html (error "No html part for this message"))
    (with-temp-file tmpfile
      (insert
       "<html>"
       "<head><meta http-equiv=\"content-type\""
       "content=\"text/html;charset=UTF-8\">"
       html))
    (browse-url (concat "file://" tmpfile))))

(add-to-list 'mu4e-view-actions
             '("View in browser" . mu4e-msgv-action-view-in-browser) t)

;; view current message's html part(translated by mu4e-html2text-command)
(defun mu4e-msgv-action-view-in-html (msg)
  "Display the message MSG in a new buffer"
  (let ((buf (get-buffer-create mu4e~view-buffer-name))
        (pref mu4e-view-prefer-html))
    ;; note: mu4e~view-mark-as-read will pseudo-recursively
    ;; call mu4e-view again by triggering mu4e~view again as
    ;; it marks the message as read
    (with-current-buffer buf
      ;;(switch-to-buffer buf)
      (setq mu4e~view-msg msg)
      (when (not (mu4e~view-mark-as-read msg))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (mu4e~delete-all-overlays)
          (setq mu4e-view-prefer-html t)
          (insert (mu4e-view-message-text msg))
          (setq mu4e-view-prefer-html pref)
          (goto-char (point-min))
          (mu4e~fontify-cited)
          (mu4e~fontify-signature)
          (mu4e~view-make-urls-clickable)	
          (mu4e~view-show-images-maybe msg)            
          ;;(when embedded (local-set-key "q" 'kill-buffer-and-window))
          (mu4e-view-mode)
          (goto-char (point-min)))))))

(add-to-list 'mu4e-view-actions
             '("toggle to HTML" . mu4e-msgv-action-view-in-html) t)

;; view the mails sent by the sender of current mail
(defun mu4e-msgv-action-sender-related-mails (msg)
  "Search all mails sent by current message's sender"
  (mu4e-headers-search
   (concat "from:" (cdar (mu4e-message-field msg :from)))))

;; define 'x' as the shortcut
(add-to-list 'mu4e-view-actions
             '("xsearch for sender" . mu4e-msgv-action-sender-related-mails) t)

;; Lync with all recipients of this mail
(defun mu4e-msgv-action-lync-with-all (msg)
  "Lync with all recipients of this mail"
  (let ((recipients '()))
    (dolist (elt (mu4e-message-field msg :from))
      (add-to-list 'recipients (cdr elt)))
    (dolist (elt (mu4e-message-field msg :to))
      (add-to-list 'recipients (cdr elt)))
    (dolist (elt (mu4e-message-field msg :cc))
      (add-to-list 'recipients (cdr elt)))
    (mu4e-goodies-lync-chat recipients)))

;; define 'L' as the shortcut
(add-to-list 'mu4e-view-actions
             '("Lync with all" . mu4e-msgv-action-lync-with-all) t)

(provide 'mu4e-goodies-actions)

