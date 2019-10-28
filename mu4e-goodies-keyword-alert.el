;;; mu4e-goodies-keyword-alert.el --- Alert of keywords under certain context  -*- lexical-binding: t; -*-

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
;; This feature provide alert of certain keywords under certain
;; context like:
;; - "attachment" appears in mail body while no attachment in the mail
;; 
;; To use this feature, you must use set mu4e-goodies-keywords like:
;; ( (regexp-of-keywords . rules-for-keywords) ...)
;; - regexp-of-keywords: regexp of keywords like: [aA]ttachment
;; - rules-for-keywords: preset rules, which are symbols like:
;;   - check-attach: check if the mail contains one or more attachments
;;   - check-cc: check if the mail has any cc recipetants
;;
;; Example of mu4e-goodies-keywords:
;; (("[aA]ttachment" . 'check-attach))
;;
;; TODO:
;; - Add check-domain, which will use blacklist/whitelist to check if the
;;   domain of email adresses are legal
;;
;;; Code:

(require 'mu4e)
(require 'hi-lock)

(defvar mu4e-goodies-rule-func
  '((check-attach . mu4e-goodies-draft-attach-p)
    (check-cc . mu4e-goodies-draft-cc-p)))

(defvar mu4e-goodies-keywords
  '(("[aA]ttachment" . check-attach)
    ("添付" . check-attach)
    ("附件" . check-attach))
  "Keywords to be alerted. An alist like:
\( (regexp-of-keywords . rules-for-keywords) ... )")

(defun mu4e-goodies-draft-attach-p ()
  "Check if current email draft has at least one attachment."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "\<#part .*filename=.*" (point-max) t)))

(defun mu4e-goodies-draft-cc-p ()
  "Check if current email draft has cc field."
  (message-fetch-field "Cc"))

(defun mu4e-goodies-search-body-subject (keyword &optional start)
  "Search for keyword in the current mail's subject and body. Return
the pos of the keyword which is a cons cell, nil if not found."
  ;; check for subject
  (save-excursion
    (if (and start (<= start (point-max)))
        (goto-char start)
      (message-goto-subject))
    (if (re-search-forward keyword (point-max) t)
        ;; check if the keyword is found in a cited line
        (let ((current-pos (point)))
          (beginning-of-line)
          (if (or (search-forward message-yank-prefix
                                  (+ (point) (length message-yank-prefix))
                                  t)
                  (search-forward message-yank-cited-prefix
                                  (+ (point) (length message-yank-cited-prefix))
                                  t))
              (mu4e-goodies-search-body-subject keyword current-pos)
            (cons (match-beginning 0) (match-end 0))))
      nil)))


(add-hook 'message-send-hook
          (defun mu4e-goodies-check-keywords ()
            (interactive "P")
            (let ((it (car mu4e-goodies-keywords))
                  (list (cdr mu4e-goodies-keywords))
                  (key-pos)
                  (msg))
              (while (and (not key-pos) it)
                (unless (and (setq key-pos (mu4e-goodies-search-body-subject (car it)))
                             (not (funcall (cdr (assoc (cdr it) mu4e-goodies-rule-func)))))
                    (setq key-pos nil)
                    (setq it (car list)
                          list (cdr list))))
              (when key-pos
                (goto-char (car key-pos))
                (overlay-put (make-overlay (car key-pos) (cdr key-pos)) 'face 'hi-yellow)
                (cond
                 ((eq (cdr it) 'check-attach) (setq msg "You may forget your attachment!"))
                 ((eq (cdr it) 'check-cc) (setq msg "You may forget your Cc!")))
                (setq msg (concat msg " Really send message?"))
                (or (y-or-n-p msg)
                    (keyboard-quit))))))

(provide 'mu4e-goodies-keyword-alert)

;;; mu4e-goodies-keyword-alert.el ends here
