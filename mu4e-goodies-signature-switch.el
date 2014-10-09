(require 'mu4e)

;; Signature swith
;;============================================================

(defvar mu4e-goodies-signatures nil
  "List of signatures used by mu4e-goodies-signature-switch, like:
(\"XXX/xxx@gmail.com\"  \"YYY/yyy@yahoo.com\" \"ZZZ/zzz@outlook.com\")")

(defun mu4e-switch-signature ()
  "Switch between signatures defined in mu4e-goodies-signatures"
  (interactive)
  (save-excursion
    (when (> (length mu4e-goodies-signatures) 0)
      (message-goto-signature)
      (let ((sig (buffer-substring (point) (point-max)))
            (next-sig nil)
            (idx 0))
        (dolist (elt mu4e-goodies-signatures next-sig)
          (setq idx (+ 1 idx))
          (when (string-equal elt sig)
            (setq next-sig (nth idx mu4e-goodies-signatures))))
        (unless next-sig
          (setq next-sig (car mu4e-goodies-signatures)))
        (delete-region (point) (point-max))
        (insert next-sig)))))

(define-key mu4e-compose-mode-map "\C-cs" 'mu4e-switch-signature)


(provide 'mu4e-goodies-signature-switch)
