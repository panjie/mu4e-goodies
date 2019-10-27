;;; mu4e-goodies-signature-switch.el --- Switch signatures

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

;; Signature swith
;;============================================================

(defcustom mu4e-goodies-signatures nil
  "List of signatures used by mu4e-goodies-signature-switch, which is an alist like:
\((signame . \"XXX/xxx@gmail.com\")  (signame . \"YYY/yyy@yahoo.com\"))"
  :type '(list (cons symbol string))
  :group 'mu4e-goodies)


(defcustom mu4e-goodies-signature-switch-rules nil
  "Rules of signature swith, which is an alist like:
\((\"regexp-to-match-address\" . signame) ...)
The rules will only apply to the first recipient's address"
  :type '(list (cons string symbol))
  :group 'mu4e-goodies)

(defun mu4e-goodies-switch-signature (&optional signame)
  "Switch between signatures defined in mu4e-goodies-signatures
if signame is not given"
  (interactive)
  (save-excursion
    (when (> (length mu4e-goodies-signatures) 0)
      (message-goto-signature)
      (let* ((sig-start (point))
             (sig-end (if (re-search-forward "\<#part" nil t)  ;; take MIME lines into consideration
                          (1- (match-beginning 0))
                        (point-max)))
             (sig (buffer-substring sig-start sig-end))
             (next-sig nil)
             (idx 0))
        (when (= sig-start (point-max)) ;; no signature found in mail
          (goto-char (point-min))
          (if (re-search-forward "\<#part" nil t)
              (goto-char (- (match-beginning 0) 1))
            (goto-char sig-start))
          (insert "--\n")
          (setq sig-start (point)
                sig-end sig-start))
        (unless (and signame (setq next-sig (cdr (assoc signame mu4e-goodies-signatures))))
          (dolist (elt mu4e-goodies-signatures next-sig)
            (setq idx (if (= (+ 1 idx) (length mu4e-goodies-signatures)) 0 (+ 1 idx)))
            (when (string-equal (cdr elt) sig)
              (setq signame (car (nth idx mu4e-goodies-signatures)))
              (setq next-sig (cdr (nth idx mu4e-goodies-signatures))))))
        (unless next-sig
          (setq signame "default signature")
          (setq next-sig (cdr (car mu4e-goodies-signatures))))
        (delete-region sig-start sig-end)
        (message "signature switched to %s" signame)
        (goto-char sig-start)
        (insert next-sig)))))

(defun mu4e-goodies-switch-signature-by-rule ()
  "Switch the draft's signature according to the rules defined in
mu4e-goodies-signature-switch-rules"
  (when mu4e-goodies-signature-switch-rules
    (let ((addr (if mu4e-compose-parent-message
                         (mu4e-message-field mu4e-compose-parent-message :from)
                       (message-field-value "To")))
          (rules mu4e-goodies-signature-switch-rules))
      (setq addr (mu4e-goodies~get-real-addr addr))
      (while (and rules (not (string-match-p (caar rules) addr)))
        (setq rules (cdr rules)))
      (when rules
        (mu4e-goodies-switch-signature (cdar rules))))))

(add-hook 'message-send-hook 'mu4e-goodies-switch-signature-by-rule)
(define-key mu4e-compose-mode-map "\C-cs" 'mu4e-goodies-switch-signature)

;; Add a menu-item in Message menu to switch signature
(when (functionp (lookup-key message-mode-map [menu-bar Message Insert\ Signature]))
  (define-key-after message-mode-map [menu-bar Message Switch\ Signature]
    '("Switch Signature" . mu4e-goodies-switch-signature) 'Insert\ Signature))


(provide 'mu4e-goodies-signature-switch)

;;; mu4e-goodies-signature-switch.el ends here
