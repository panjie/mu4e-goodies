;; Copyright (C) 2019  Pan Jie (panjie@gmail.com)

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;---
;; highlight specified keywords in header view
;;---

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
