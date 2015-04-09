;; Copyright (C) 2014  Pan Jie (panjie@gmail.com)

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

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
;; TODO: not works at all
(defun mu4e-msgv-action-lync-with-all (msg)
  "Lync with all recipients of this mail"
  (mu4e-goodies-lync-chat (cdar (mu4e-message-field msg :from))))
  
  ;; (let ((recipients '()))
  ;;   (dolist (elt (mu4e-message-field msg :from))
  ;;     (add-to-list 'recipients (cdr elt)))
  ;;   (dolist (elt (mu4e-message-field msg :to))
  ;;     (add-to-list 'recipients (cdr elt)))
  ;;   (dolist (elt (mu4e-message-field msg :cc))
  ;;     (add-to-list 'recipients (cdr elt)))
  ;;   (mu4e-goodies-lync-chat recipients)))

;; define 'L' as the shortcut
(add-to-list 'mu4e-view-actions
             '("Lync with all" . mu4e-msgv-action-lync-with-all) t)


;;
;; Convert an email to a todo item
;;
;; - Use mu4e-goodies-todo-file and mu4e-goodies-todo-parent-entry to
;;   specifify the where the item will be inserted
;;
(require 'org)
(require 'org-mu4e)

;; Create a todo entry in the specified subtree of specified org file
(defcustom mu4e-goodies-org-file nil
  "default org file where the mail-based todo/meeting will be inserted"
  :group 'mu4e-goodies)

(defcustom mu4e-goodies-todo-parent-entry nil
  "The default subtree entry where the mail-based todo item will be inserted"
  :group 'mu4e-goodies)

(defcustom mu4e-goodies-meeting-parent-entry nil
  "The default subtree entry where the mail-based meeting item will be inserted"
  :group 'mu4e-goodies)

(defvar mu4e-goodies-recent-org-file nil
  "recent org-todo file used")

(defvar mu4e-goodies-recent-todo-parent-entry nil
  "recent todo parent entry where the new item will be inserted")

(defvar mu4e-goodies-recent-meeting-parent-entry nil
  "recent meeting parent entry where the new item will be inserted")

(defun mu4e-goodies-insert-item (todop file entry title &optional ts content)
  "Insert an new todo/meeting heading with the specified title in the
subtree of file's entry with the content."
  (with-temp-file file
    (org-mode)
    (let ((buf (current-buffer))
          entry-marker)
      ;; load the content of file into temp buffer
      (goto-char (point-min))
      (insert-file-contents file)
      (when (setq entry-marker (org-find-exact-headline-in-buffer entry))
        (goto-char (marker-position entry-marker))
        (goto-char (line-end-position))
        (if todop
            (org-insert-todo-subheading 1)
          (org-insert-subheading 1))
        (insert title)
        (when ts
          (if todop
              (progn (org-deadline 1 (replace-regexp-in-string "<\\(.+-.+-[^ ]+\\) .+>" "\\1" ts))
                     (goto-char (line-end-position 2)))
            (insert ts)))
        (insert "\n")
        (when content
          ;;TODO: indent the content
          (insert content)
          (insert "\n"))
        ;; add t here because insert always return nil
        t))))

;; TODO:
;; - add multi-language support
;; - auto save/revert if the org file is already opened
(defun mu4e-goodies-add-org-item (&optional istodo item-title item-link)
  "Add org meeting/todo anywhere you want from minibuffer"
  (interactive)
  (let ((title item-title)
        (link item-link)
        file entry body ts)
    (setq title (read-string "Title: " title))
    (unless (setq file (or mu4e-goodies-org-file mu4e-goodies-recent-org-file))
          (setq file (read-file-name "Org file name: " nil nil nil (car org-agenda-files))))
    (unless (setq entry (if istodo
                            (or mu4e-goodies-todo-parent-entry
                                mu4e-goodies-recent-todo-parent-entry)
                          (or mu4e-goodies-meeting-parent-entry 
                              mu4e-goodies-recent-meeting-parent-entry)))
      (setq entry (read-string "Parent heading: " (if istodo "Tasks" "Meetings") nil nil)))
    (setq ts (with-temp-buffer (org-time-stamp 1)(buffer-string)))
    ;; remember as recent file
    (when (mu4e-goodies-insert-item istodo file entry title ts link)
      (setq mu4e-goodies-recent-org-file file
            mu4e-goodies-recent-meeting-parent-entry entry)
      (message "%s [%s] created successfully" (if istodo "Todo" "Meeting") title))))

(defun mu4e-goodies-action-make-todo (msg)
  "Make an org todo item based on current mail"
  (mu4e-goodies-add-org-item t (mu4e-message-field msg :subject)))

(defun mu4e-goodies-action-make-meeting (msg)
  "Make an org meeting item based on current mail"
  (mu4e-goodies-add-org-item nil (mu4e-message-field msg :subject)))

(add-to-list 'mu4e-view-actions
             '("new todo from this mail" . mu4e-goodies-action-make-todo) t)
(add-to-list 'mu4e-view-actions
             '("meeting from this mail" . mu4e-goodies-action-make-meeting) t)


(provide 'mu4e-goodies-actions)

