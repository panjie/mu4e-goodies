;;; mu4e-goodies-hacks.el --- Useful hacks for mu4e  -*- lexical-binding: t; -*-

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

;; Usefuly hacks for mu4e

;;; Code:



(require 'mu4e)

;;
;; Set hl-line-sticky-flag to t so that message in header view will
;; still be high lighted even when viewing the message
;;

(setq hl-line-sticky-flag t)

;;
;; Add the following menu item to the Message menu:
;; - Insert the captured message
;; 
(when (functionp (lookup-key message-mode-map [menu-bar Message Insert\ File\ Marked...]))
  (define-key-after message-mode-map [menu-bar Message Insert\ Caputured\ Message] 
    '("Insert Captured Message" . mu4e-compose-attach-captured-message) 'Insert\ File\ Marked...))


;;
;; Allow a mu4e-view buffer detached from mu4e-header so that it will be
;; retained in a seperated window or frame.
;;
(defun mu4e-goodies-detach-msg-view (&optional towin focusnew)  
  "Detach the current mu4e-view buffer from header to a new frame or window.

If towin is t, the detached message view will be presented in a
splitted window. Otherwise it will be presented in a new frame.

If focusnew is t, the new window/frame will be focused"
  (interactive)
  (let* ((buf (current-buffer))
         (frm (selected-frame))
         (new-win nil)
         (new-frm nil))
    (when (string= (buffer-name buf) mu4e~view-buffer-name)
      ;; rename it so that it will not be found by mu4e-view
      (rename-buffer (concat "*mu4e-view*" (mu4e-msg-field (mu4e-message-at-point t) :subject) "*") t)
      (if towin
          (setq new-win (split-window-below))
        (progn
          (setq new-frm (make-frame))
          (select-frame-set-input-focus frm)))
      (mu4e-view (mu4e-message-at-point t))
      (when focusnew
        (if towin
            (select-window new-win)
          (select-frame-set-input-focus new-frm))))))

(define-key 'mu4e-view-mode-map "\'" 'mu4e-goodies-detach-msg-view)
(define-key 'mu4e-view-mode-map "\"" (lambda () (interactive) (mu4e-goodies-detach-msg-view t nil)))

;;
;; Always put attachments to the bottom of email
;; http://mbork.pl/2015-11-28_Fixing_mml-attach-file_using_advice
;;
(defun mml-attach-file--go-to-eob (orig-fun &rest args)
  "Go to the end of buffer before attaching files."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (apply orig-fun args))))

(advice-add 'mml-attach-file :around #'mml-attach-file--go-to-eob)
(advice-add 'mml-dnd-attach-file :around #'mml-attach-file--go-to-eob)

;;
;; Quickly save last query to mu4e-bookmarks and save to custom file
;;
(defun mu4e-goodies-save-last-query-to-bookmarks (&optional notsave)
  "Save last query to mu4e-bookmarks and save to custom file."
  (interactive "P")
  (when (> (length (mu4e-last-query)) 0)
    (let ((name "")
          (key nil))
      ;; TODO: check if name and key are already exist in mu4e-bookmarks
      (while (= (length name) 0)
        (setq name (read-string "Bookmark name: ")))
      (unless (and key (= ?w (char-syntax key)))
        (setq key (read-char "Key: ")))
      (mu4e-bookmark-define (mu4e-last-query) name key)
      ;; save it
      (unless notsave
        (mu4e-message "Saving bookmark %s..." name)
        (customize-save-variable 'mu4e-bookmarks mu4e-bookmarks)))))

(define-key 'mu4e-headers-mode-map "K" 'mu4e-goodies-save-last-query-to-bookmarks)

;;
;; Quickly delete one email address in TO/CC/BCC fields
;;
(defun mu4e-goodies-delete-address ()
  "Quickly delete one email address in TO/CC/BCC fields."
  (interactive)
  (let ((addr-begin nil)
        (addr-end nil))
    ;; Search for the beginning of the addresses, which maybe like:
    ;; - "XXX, YYY/ZZZ" <abc@def.com>
    ;; - abc@def.com
    (if (search-backward "," nil t)
        (if (re-search-backward "\"[^,]+" nil t)
            ;; "XXX, YYY/ZZZ" <abc.def.com>
            ;; ^---^
            (setq addr-begin (point))
          ;; ..., abc@def.com
          ;;    ^
          (setq addr-begin (+ 1 (point))))
      ;; To: abc@def.com
      ;; somewhere in the first of address list
      (setq addr-begin (+ 4 (search-backward "To: " nil t))))
    (goto-char addr-begin)
    (if (re-search-forward "[^@]+@[^.]+\.[^,]+," nil t)
        ;; "XXX, YYY/ZZZ" <abc.def.com>,
        ;;                             ^
        (setq addr-end (point))
      ;; at the end of address list
      (setq addr-end (line-end-position)))
    (delete-region addr-begin addr-end)))

(defun mu4e-goodies-wrapped-delete (orig-func)
  "When in address related fields, call mu4e-goodies-delete-address. 
Otherwise call orig-func which is usually \\<\M-d>"
  (interactive)
  (let ((eoh ;; end-of-headers
        (save-excursion
          (goto-char (point-min))
          (search-forward-regexp mail-header-separator nil t))))
    (if (and eoh (> eoh (point)) (mail-abbrev-in-expansion-header-p))
        (mu4e-goodies-delete-address)
      (funcall orig-func))))

;;
;; Fontify signature
;;

(defun mu4e~fontify-signature ()
  "Give the message signatures a distinctive color. This is used in
the view and compose modes."
  (let ((inhibit-read-only t))
    (save-excursion
      ;; give the footer a different color...
      (goto-char (point-min))
      (let* ((p (re-search-forward "^---* *$" nil t))
             (q nil))
        (when p
          (setq p (match-beginning 0))
          (setq q (re-search-forward "^ *$" nil t))
          (add-text-properties p
                               (or q (point-max))
                               '(face mu4e-footer-face)))))))

;;
;; Quickly add/remove/search for flag
;;

(defun mu4e-goodies~hacks-flag-unflag ()
  (interactive)
  (let* ((msg (mu4e-message-at-point))
         (docid (mu4e-message-field msg :docid))
         (flags (mu4e-message-field msg :flags)))
    (if (memq 'flagged flags)
        (progn
          (mu4e-message "unflagging...")
          (mu4e~proc-move docid nil "-F-N"))
      (progn
        (mu4e-message "flagging...")
        (mu4e~proc-move docid nil "+F-u-N")))))

(define-key mu4e-headers-mode-map (kbd "M") 'mu4e-goodies~hacks-flag-unflag)
(define-key mu4e-view-mode-map (kbd "M") 'mu4e-goodies~hacks-flag-unflag)
(define-key mu4e-headers-mode-map (kbd "k") (lambda ()
                                              (interactive)
                                              (mu4e~headers-search-execute "flag:f" t)))


;;
;; Remove extra blanklines generated by html2text command like
;; mu4e-shr2text.
;;

(defun mu4e-goodies-message-delete-html-extra-blanklines (msg body)
  "Delete extra blank lines generated by html2text command/functions
like mu4e-shr2text"
  (if mu4e~message-body-html
      (with-temp-buffer
        (insert body)
        (goto-char (point-min))
        (flush-lines "^$")
        (buffer-string))
    body))

(add-to-list 'mu4e-message-body-rewrite-functions
             'mu4e-goodies-message-delete-html-extra-blanklines)

;;
;; Xapian, the search engine of mu has a poor support of CJK characters,
;; which causes only query contains no more than 2 CJK characters works.
;; 
;; https://researchmap.jp/?page_id=457
;;
;; This workaroud breaks any CJK words longer than 2 characters into
;; combines of bi-grams. Example: 我爱你 -> (我爱 爱你)
;;
(defun mu4e-goodies~break-cjk-word (word)
  "Break CJK word into list of bi-grams like: 我爱你 -> 我爱 爱你"
  (if (or (<= (length word) 2)
          (equal (length word) (string-bytes word))) ; only ascii chars
      word
    (let ((pos nil)
          (char-list nil)
          (br-word nil))
      (if (setq pos (string-match ":" word))     ; like: "s:abc"
          (concat (substring word 0 (+ 1 pos)) 
                  (mu4e-goodies~break-cjk-word (substring word (+ 1 pos))))
        (if (memq 'ascii (find-charset-string word)) ; ascii mixed with others like: abcあいう
            word
          (progn 
            (setq char-list (split-string word "" t))
            (while (cdr char-list)
              (setq br-word (concat br-word (concat (car char-list) (cadr char-list)) " "))
              (setq char-list (cdr char-list)))
            br-word))))))

(defun mu4e-goodies~break-cjk-query (expr)
  "Break CJK strings into bi-grams in query."
  (let ((word-list (split-string expr " " t))
        (new ""))
    (dolist (word word-list new)
      (setq new (concat new (mu4e-goodies~break-cjk-word word) " ")))))

(setq mu4e-query-rewrite-function 'mu4e-goodies~break-cjk-query)

(provide 'mu4e-goodies-hacks)

;;; mu4e-goodies-hacks.el ends here
