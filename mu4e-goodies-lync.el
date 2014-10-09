(require 'mu4e)
(require 'browse-url)

(defun mu4e-goodies-lync-chat (&optional email)
  "Lync with the email address at point if parameter email is not given"
  (interactive)
  (browse-url (concat "sip:" (if email
                                 email
                               (get-text-property (point) 'email)))))

(define-key mu4e-view-contacts-header-keymap "L" 'mu4e-lync-chat)

(provide 'mu4e-goodies-lync)
