;; Copyright (C) 2014  Pan Jie (panjie@gmail.com)

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

;; Signature swith
;;============================================================

(defvar mu4e-goodies-signatures nil
  "List of signatures used by mu4e-goodies-signature-switch,
  which is an alist like:
((\"sig name\" . \"XXX/xxx@gmail.com\")  (\"sig name\" . \"YYY/yyy@yahoo.com\"))")

(defvar mu4e-goodies-signature-switch-rules nil
  "Rules of signature swith, which is an alist like:
((\"rules-of-regexp\" . \"sig name\") ...)
The rules will only apply to the first recipient's address")

(defun mu4e-goodies-switch-signature (&optional signame)
  "Switch between signatures defined in mu4e-goodies-signatures
if signame is not given"
  (interactive)
  (save-excursion
    (when (> (length mu4e-goodies-signatures) 0)
      (message-goto-signature)
      (let ((sig (buffer-substring (point) (point-max)))
            (next-sig nil)
            (idx 0))
        (unless (and signame (setq next-sig (cdr (assoc signame mu4e-goodies-signatures))))
          (dolist (elt mu4e-goodies-signatures next-sig)
            (setq idx (if (= (+ 1 idx) (length mu4e-goodies-signatures)) 0 (+ 1 idx)))
            (when (string-equal (cdr elt) sig)
              (setq signame (car (nth idx mu4e-goodies-signatures)))
              (setq next-sig (cdr (nth idx mu4e-goodies-signatures))))))
        (unless next-sig
          (setq signame "mu4e's default signature")
          (setq next-sig (cdr (car mu4e-goodies-signatures))))
        (delete-region (point) (point-max))
        (message "signature switched to %s" signame)
        (insert next-sig)))))

(defun mu4e-goodies-switch-signature-by-rule ()
  "Switch the draft's signature according to the rules defined in
  mu4e-goodies-signature-switch-rules"
  (let ((msg mu4e-compose-parent-message)
        (rules mu4e-goodies-signature-switch-rules))
    (while (and rules 
                (not (mu4e-message-contact-field-matches msg
                                                         :from
                                                         (caar rules))))
      (setq rules (cdr rules)))
    (when rules
      (mu4e-goodies-switch-signature (cdar rules)))))


(define-key mu4e-compose-mode-map "\C-cs" 'mu4e-goodies-switch-signature)

;; Add a menu-item in Message menu to switch signature
(when (functionp (lookup-key message-mode-map [menu-bar Message Insert\ Signature]))
  (define-key-after message-mode-map [menu-bar Message Switch\ Signature]
    '("Switch Signature" . mu4e-goodies-switch-signature) 'Insert\ Signature))




(provide 'mu4e-goodies-signature-switch)
