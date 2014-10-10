(require 'mu4e)
(require 'browse-url)

(defun mu4e-goodies-lync-chat (&optional emails)
  "Lync with the email address at point if parameter email is not given"
  (interactive)
  (browse-url (if (and emails (> (length emails) 0))
                  (let ((uri "im:"))
                    (dolist (email emails)
                      (unless (string= email user-mail-address) 
                        (setq uri (concat uri "<sip:" email ">"))))
                    (mu4e-message "Lync with %s" uri)
                    uri)
                (concat "sip:" (get-text-property (point) 'email)))))

(define-key mu4e-view-contacts-header-keymap "L" 'mu4e-goodies-lync-chat)

(provide 'mu4e-goodies-lync)
