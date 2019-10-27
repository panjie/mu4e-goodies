;;; mu4e-goodies-header-color.el --- highlight specified keywords in header view

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
  "Special keywords to be highlighted in header view.
Example: 
((:from . (\"vip@company.com\" ...))(:subject . (\"NOTICE\" ...)))"
  :type 'alist
  :group 'mu4e-goodies)

(defface mu4e-goodies-face-special-keywords
  '((t :inverse-video t))
  "Face for showing special mails in header view"
  :group 'mu4e-goodies)

(defun mu4e-goodies-header-add-color (msg field val width)
  "highlight specified keywords in header view"
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

(add-to-list 'mu4e~headers-field-handler-functions 'mu4e-goodies-header-add-color)

(provide 'mu4e-goodies-header-color)

;;; end of mu4e-goodies-header-color.el
