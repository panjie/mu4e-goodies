;;; mu4e-goodies-header-color.el --- highlight specified keywords in header view  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2021  Pan Jie

;; Author: Pan Jie <panjie@gmail.com>
;; Created: 2014-10-8
;; Updated: 2021-10-18
;; Version: 2.0
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

;; for mu >= 1.5, str will be the return of mu4e~headers-field-value
;; for mu < 1.5. str will be nil
(defun mu4e-goodies-header-add-color-handler (msg field f-v str)
  "Highlight specified keywords in header view."
  (if (assoc field mu4e-goodies-special-field-keywords)
      (let* ((keywords (cdr (assoc field mu4e-goodies-special-field-keywords)))
             (val (or str f-v)))
        (cond ((stringp f-v)            ; may be a subject or something else
               (dolist (kw keywords val)
                 (when (string-match-p kw f-v)
                   (setq val (propertize val 'face 'mu4e-goodies-face-special-keywords)))))
              ((and (eq field :from) (listp f-v) (member (cdar f-v) keywords)) ; from field whose val should be like: (("XXX" . "xxx@yyy.com") ...)
               (if (stringp val)
                   (propertize val 'face 'mu4e-goodies-face-special-keywords) ; mu >= 1.5
                 (list (cons (propertize (caar val) 'face 'mu4e-goodies-face-special-keywords) ; mu < 1.5
                             (propertize (cdar val) 'face 'mu4e-goodies-face-special-keywords)))))
              (t val)))
    (or str f-v)))


(cond ((functionp 'mu4e~headers-field-value) ; for mu>=1.5
       (add-to-list 'mu4e-goodies~header-handlers 'mu4e-goodies-header-add-color-handler))
      ((listp 'mu4e~headers-field-handler-functions) ; for mu<1.5
       (add-to-list 'mu4e~headers-field-handler-functions (lambda (msg field val width)
                                                            "" (mu4e-goodies-header-add-color-handler msg field val nil))))
      (t nil))

(provide 'mu4e-goodies-header-color)

;;; mu4e-goodies-header-color.el ends here
