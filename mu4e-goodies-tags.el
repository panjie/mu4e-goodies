;;; mu4e-goodies-tags.el --- Provide useful functions to use tags in mu4e  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2019  Pan Jie

;; Author: Pan Jie <panjie@gmail.com>
;; Created: 2014-10-8
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: email tools
;; URL: https://github.com/panjie/mu4e-goodies
;; License: https://github.com/panjie/mu4e-goodies/LICENSE

;; This file is not a part of GNU Emacs.

;;; Code:

(require 'mu4e)
(require 'mu4e-goodies-utils)


;;
;; Provide some usable functions to use tags in mu4e
;;


;; Tags for emails (from info pages of mu4e)
;;--------------------------------------------------
(add-to-list 'mu4e-marks
             '(tag
               :char       ("g" . " ")
               :prompt     "gtag"
               :ask-target (lambda () (read-string "What tag do you want to add/remove(+/-): "))
               :action      (lambda (docid msg target)
                              (mu4e-action-retag-message msg target))))
(mu4e~headers-defun-mark-for tag)
(define-key mu4e-headers-mode-map (kbd "G") 'mu4e-headers-mark-for-tag)
(define-key-after (lookup-key mu4e-headers-mode-map [menu-bar headers])
  [mark-tag] '("Mark for tag" . mu4e-headers-mark-for-tag) 'mark-pattern)


;; actions to add tags
(add-to-list 'mu4e-view-actions
             '("add/remove tags" . mu4e-action-retag-message) t)


;; Quickly add/remove/search tag (named QT**) in header/message view
;;--------------------------------------------------
(defvar mu4e-goodies~quick-tag "QT**"
  "Quick tag.")

(defun mu4e-goodies-add-del-quick-tag ()
  "Quickly add/del tags."
  (interactive)
  (let* ((msg (mu4e-message-at-point))
         (oldtags (mu4e-message-field msg :tags)))
    (if (member mu4e-goodies~quick-tag oldtags)
        (mu4e-action-retag-message msg (concat "-" mu4e-goodies~quick-tag))
      (mu4e-action-retag-message msg (concat "+" mu4e-goodies~quick-tag)))))

;; flag maybe a better choice than tag so the following code is useless
;; (define-key mu4e-headers-mode-map (kbd "M") 'mu4e-goodies-add-del-quick-tag)
;; (define-key mu4e-view-mode-map (kbd "M") 'mu4e-goodies-add-del-quick-tag)
;; (define-key mu4e-headers-mode-map (kbd "k") (lambda ()
;;                                               (interactive)
;;                                               (mu4e~headers-search-execute (concat "tag:" mu4e-goodies~quick-tag) t)))


;;
;; Show tags in the header view
;;
(defface mu4e-goodies-face-tags
  '((((class color) (background light)) :weight bold :foreground "#8CD0D3")
    (((class color) (background  dark)) :weight bold :foreground "#8CD0D3"))
  "Face for show tags in header view."
  :group 'mu4e-goodies)

(defun mu4e-goodies-header-add-tags-handler (msg field f-v str)
  "Add tags to header view's subject field like: [TAG][TAG] subject..."
  (let* ((val (or str f-v)))
    (if (eq field :subject)
        (let ((tags (mu4e-message-field msg :tags)))
          (if tags
              (setq val (concat
                         (mapconcat (function (lambda (x) (propertize (concat "[" x "]") 'face 'mu4e-goodies-face-tags)))
                                    tags "")
                         " "
                         val))
            val))
          val)))

(cond ((functionp 'mu4e~headers-field-value) ; for mu>=1.5
       (add-to-list 'mu4e-goodies~header-handlers 'mu4e-goodies-header-add-tags-handler))
      ((listp 'mu4e~headers-field-handler-functions) ; for mu<1.5
       (add-to-list 'mu4e~headers-field-handler-functions (lambda (msg field val width)
                                                            "" (mu4e-goodies-header-add-tags-handler msg field val nil))))
      (t nil))

(provide 'mu4e-goodies-tags)

;;; mu4e-goodies-tags.el ends here
