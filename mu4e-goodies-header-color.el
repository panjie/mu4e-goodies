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

(require 'mu4e-goodies-utils)

(defvar mu4e-goodies-special-field-keywords
  '((:from . (nagakura@cn.fujitsu.com))))

;;
;; Show tags in the header view
;;
(defface mu4e-goodies-face-special-keywords
  '((((class color) (background light)) :weight bold :foreground "#8CD0D3")
    (((class color) (background  dark)) :weight bold :foreground "#8CD0D3"))
  "Face for showing special mails in header view"
  :group 'mu4e-goodies)

(defun mu4e-goodies-header-add-color (msg field val width)
  "Add tags to header view's subject field like: [TAG] Subject..."
  (if (assoc field mu4e-goodies-special-field-keywords)
      (let ((pair (assoc field mu4e-goodies-special-field-keywords))
            (realaddr (when (member field '(:from :to :cc :bcc)) ; address fields
                        (mu4e-goodies~get-real-addr val))))
        (if (member (if realaddr realaddr val) (cdr pair))
            (propertize val 'face 'mu4e-goodies-face-special-keywords)
          val))
    val))
                              
(add-to-list 'mu4e~headers-field-handler-functions 'mu4e-goodies-header-add-color)

(provide 'mu4e-goodies-header-color)
