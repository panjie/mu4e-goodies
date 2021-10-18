;;; mu4e-goodies-header-color.el --- highlight specified keywords in header view  -*- lexical-binding: t; -*-

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

(require 'mu4e-goodies-utils)

(defcustom mu4e-goodies-special-field-keywords nil
  "Special keywords to be highlighted in header view. The keywords may be
email addresses or words in the subjects or both.
Example:
\((:from . (\"vip@company.com\" ...))(:subject . (\"NOTICE\" ...)))"
  :type 'alist
  :group 'mu4e-goodies)

(defface mu4e-goodies-face-special-keywords
  '((t :inverse-video t))
  "Face for showing special mails in header view"
  :group 'mu4e-goodies)

;; NOTICE: this extension is relying heavily on the internal implementation of mu4e-headers

;; For mu < 1.5
(defun mu4e-goodies-header-add-color (msg field val width)
  "Highlight specified keywords in header view."
  (if (assoc field mu4e-goodies-special-field-keywords)
      (let* ((keywords (cdr (assoc field mu4e-goodies-special-field-keywords))))
        (cond ((stringp val)            ; may be a subject or something else
               (dolist (kw keywords val)
                 (when (string-match-p kw val)
                   (setq val (propertize val 'face 'mu4e-goodies-face-special-keywords)))))
              ((and (eq field :from) (listp val) (member (cdar val) keywords)) ; from field whose val should be like: (("XXX" . "xxx@yyy.com") ...)
               (list (cons (propertize (caar val) 'face 'mu4e-goodies-face-special-keywords)
                           (propertize (cdar val) 'face 'mu4e-goodies-face-special-keywords))))
              (t val)))
    val))


;; For mu >= 1.5 and depends on the implementation of mu4e~headers-field-value
(defun mu4e-goodies-header-add-color-advice (orig-func &rest args)
  "Highlight specified keywords in header view."
  (if (assoc (cadr args) mu4e-goodies-special-field-keywords)
      (let* ((msg (car args))
             (field (cadr args))
             (field-val (mu4e-message-field msg field))
             (val (apply orig-func args)))  ;; should be a string
        (let* ((keywords (cdr (assoc field mu4e-goodies-special-field-keywords))))
          (cond ((stringp field-val)            ; may be a subject or something else
                 (dolist (kw keywords val)
                   (when (string-match-p kw field-val)
                     (setq val (propertize val 'face 'mu4e-goodies-face-special-keywords)))))
                ((and (eq field :from) (listp field-val) (member (cdar field-val) keywords)) ; from field whose val should be like: (("XXX" . "xxx@yyy.com") ...)
                 (propertize val 'face 'mu4e-goodies-face-special-keywords))
                (t val))))
    (apply orig-func args)))

(cond ((functionp 'mu4e~headers-field-value) ; for mu>=1.5
       (advice-add 'mu4e~headers-field-value :around #'mu4e-goodies-header-add-color-advice))
      ((listp 'mu4e~headers-field-handler-functions) ; for mu<1.5
       (add-to-list 'mu4e~headers-field-handler-functions 'mu4e-goodies-header-add-color))
      (t nil))


(provide 'mu4e-goodies-header-color)

;;; mu4e-goodies-header-color.el ends here
