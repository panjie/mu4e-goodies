;; Copyright (C) 2017  Pan Jie (panjie@gmail.com)

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


;;
;; Provide some usable functions to use tags in mu4e
;;


;; Tags for emails (from info pages of mu4e)
;;--------------------------------------------------
(add-to-list 'mu4e-marks
             '(tag
               :char       ("g" . " ")
               :prompt     "gtag"
               :ask-target (lambda () (read-string "What tag do you want to add: "))
               :action      (lambda (docid msg target)
                              (mu4e-action-retag-message msg (concat "+" target)))))
(mu4e~headers-defun-mark-for tag)
(define-key mu4e-headers-mode-map (kbd "G") 'mu4e-headers-mark-for-tag)
(define-key-after (lookup-key mu4e-headers-mode-map [menu-bar headers])
  [mark-tag] '("Mark for tag" . mu4e-headers-mark-for-tag) 'mark-pattern)


;; actions to add tags
(add-to-list 'mu4e-view-actions
             '("add tags" . mu4e-action-retag-message) t)


;; Quickly add/remove/search tag (named QT**) in header/message view
;;--------------------------------------------------
(defvar mu4e-goodies~quick-tag "QT**"
  "Quick tag")

(defun mu4e-goodies-add-del-quick-tag ()
  (interactive)
  (let* ((msg (mu4e-message-at-point))
         (oldtags (mu4e-message-field msg :tags)))
    (if (member mu4e-goodies~quick-tag oldtags)
        (mu4e-action-retag-message msg (concat "-" mu4e-goodies~quick-tag))
      (mu4e-action-retag-message msg (concat "+" mu4e-goodies~quick-tag)))))

;; flag maybe a better choice than tag
;; (define-key mu4e-headers-mode-map (kbd "M") 'mu4e-goodies-add-del-quick-tag)
;; (define-key mu4e-view-mode-map (kbd "M") 'mu4e-goodies-add-del-quick-tag)
;; (define-key mu4e-headers-mode-map (kbd "k") (lambda ()
;;                                               (interactive)
;;                                               (mu4e~headers-search-execute (concat "tag:" mu4e-goodies~quick-tag) t)))

(provide 'mu4e-goodies-tags)


