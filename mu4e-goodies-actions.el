;;; mu4e-goodies-actions.el --- Useful actions for mu4e

;; Copyright (C) 2014-2019  Pan Jie

;; Author: Pan Jie <panjie@gmail.com>
;; Created: 2014-10-8
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: email tools
;; URL: https://github.com/panjie/mu4e-goodies
;; License: https://github.com/panjie/mu4e-goodies/LICENSE

;; This file is not a part of GNU Emacs.


;;; Commentary:
;; 

;;; Code:

(require 'mu4e)

;; Actions
;;--------------------------------------------------

;; show current message's html part in browser
(defun mu4e-msgv-action-view-in-browser (msg)
  "View the body of the msg in a web browser."
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


;; view the mails sent by the sender of current mail
(defun mu4e-msgv-action-sender-related-mails (msg)
  "Search all mails sent by current message's sender."
  (mu4e-headers-search
   (concat "from:" (cdar (mu4e-message-field msg :from)))))

;; define 'x' as the shortcut
(add-to-list 'mu4e-view-actions
             '("xsearch for sender" . mu4e-msgv-action-sender-related-mails) t)


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
  "Default org file where the mail-based todo/meeting will be inserted."
  :group 'mu4e-goodies)

(defcustom mu4e-goodies-todo-parent-entry nil
  "The default subtree entry where the mail-based todo item will be inserted."
  :group 'mu4e-goodies)

(defcustom mu4e-goodies-meeting-parent-entry nil
  "The default subtree entry where the mail-based meeting item will be inserted."
  :group 'mu4e-goodies)

(defvar mu4e-goodies-recent-org-file nil
  "Recent org-todo file used.")

(defvar mu4e-goodies-recent-todo-parent-entry nil
  "Recent todo parent entry where the new item will be inserted.")

(defvar mu4e-goodies-recent-meeting-parent-entry nil
  "Recent meeting parent entry where the new item will be inserted.")

(defun mu4e-goodies-insert-item (todop file entry title &optional ts content)
  "Insert an new todo/meeting heading with the specified title in
the subtree of file's entry with the content."
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
          (indent-according-to-mode)
          (insert content)
          (insert "\n"))
        ;; add t here because insert always return nil
        t))))

;; TODO:
;; - add multi-language support
;; - auto save/revert if the org file is already opened
(defun mu4e-goodies-add-org-item (&optional istodo item-title item-link)
  "Add org meeting/todo anywhere you want from minibuffer."
  (interactive)
  (let ((title item-title)
        (link item-link)
        file entry body ts)
    (setq title (read-string (format "%s title: " (if istodo "Task" "Meeting"))
                             title))
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

(defun mu4e-goodies-create-msg-link (msg)
  "Create a org-link to msg."
  (let* ((msgid (or (plist-get msg :message-id) "<none>"))
         (from  (or (plist-get msg :from) '(("none" . "none"))))
         (fromname (car (car from)))
         (fromaddress (cdr (car from)))
         (to  (or (plist-get msg :to) '(("none" . "none"))))
         (toname (car (car to)))
         (toaddress (cdr (car to)))
         (fromto (if (mu4e-user-mail-address-p fromaddress)
                     (format "to %s <%s>" toname toaddress)
                   (format "from %s <%s>" fromname fromaddress)))
         (date (plist-get msg :date))
         (date-ts (format-time-string (org-time-stamp-format t) date))
         (date-ts-ia (format-time-string (org-time-stamp-format t t) date))
         (subject  (or (plist-get msg :subject) "<none>"))
         link)
    (org-store-link-props :type "mu4e" :link link
                          :message-id msgid)
    (setq link (concat "mu4e:msgid:" msgid))
    (org-add-link-props :link link
                        :to (format "%s <%s>" toname toaddress)
                        :toname toname
                        :toaddress toaddress
                        :from (format "%s <%s>" fromname fromaddress)
                        :fromname fromname
                        :fromaddress fromaddress
                        :fromto fromto
                        :date date-ts-ia
                        :date-timestamp date-ts
                        :date-timestamp-inactive date-ts-ia
                        :subject subject
                        :description (funcall org-mu4e-link-desc-func msg))
    (concat "[[" link "][" (funcall org-mu4e-link-desc-func msg) "]]")))

(defun mu4e-goodies-action-make-todo (msg)
  "Make an org todo item based on current mail."
  (mu4e-goodies-add-org-item t (mu4e-message-field msg :subject)
                             (mu4e-goodies-create-msg-link msg)))

(defun mu4e-goodies-action-make-meeting (msg)
  "Make an org meeting item based on current mail."
  (mu4e-goodies-add-org-item nil (mu4e-message-field msg :subject)
                             (mu4e-goodies-create-msg-link msg)))

(defun mu4e-goodies-action-copy-org-link (msg)
  "Copy org link of the message to clipboard."
  (with-temp-buffer
    (insert (mu4e-goodies-create-msg-link msg))
    (clipboard-kill-ring-save (point-min) (point-max))))


(add-to-list 'mu4e-view-actions
             '("todo" . mu4e-goodies-action-make-todo) t)
(add-to-list 'mu4e-view-actions
             '("meeting" . mu4e-goodies-action-make-meeting) t)
(add-to-list 'mu4e-view-actions
             '("org link" . mu4e-goodies-action-copy-org-link) t)


(provide 'mu4e-goodies-actions)

;;; mu4e-goodies-actions.el ends here
