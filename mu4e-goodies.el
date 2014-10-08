(require 'mu4e)

;; Add indicators to mode-line to show the status of 3 variables:
;; - mu4e-headers-full-search
;; - mu4e-headers-show-threads: the swither when you push P
;; - mu4e-headers-include-related: the swither when you push W
(add-hook 'mu4e-headers-found-hook (lambda ()
                                     (setq-local global-mode-string
                                                 (propertize
                                                  (concat mu4e~headers-last-query
                                                          "   ["
                                                          (if mu4e-headers-full-search "F" "-")
                                                          (if mu4e-headers-show-threads "P" "-")
                                                          (if mu4e-headers-include-related "W" "-")
                                                          "]")
                                                  'face 'mu4e-title-face))))

(provide mu4e-goodies-indicators)


;; Signature swith

(defvar mu4e-compose-signatures nil
  "List of signatures used by mu4e-goodies-signature-switch, like:
(\"XXX/xxx@gmail.com\"  \"YYY/yyy@yahoo.com\" \"ZZZ/zzz@outlook.com\")")

(defun mu4e-switch-signature ()
  "Switch between signatures defined in mu4e-compose-signatures"
  (interactive)
  (save-excursion
    (when (> (length mu4e-compose-signatures) 0)
      (message-goto-signature)
      (let ((sig (buffer-substring (point) (point-max)))
            (next-sig nil)
            (idx 0))
        (dolist (elt mu4e-compose-signatures next-sig)
          (setq idx (+ 1 idx))
          (when (string-equal elt sig)
            (setq next-sig (nth idx mu4e-compose-signatures))))
        (unless next-sig
          (setq next-sig (car mu4e-compose-signatures)))
        (delete-region (point) (point-max))
        (insert next-sig)))))

(define-key mu4e-compose-mode-map "\C-cs" 'mu4e-switch-signature)

(provide mu4e-goodies-signature-switch)
