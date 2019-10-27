;;; mu4e-goodies-compose-with-attachments.el --- Provide helper functions to create shortcuts in Finder/Explorer to send files using mu4e

;; Copyright (C) 2014-2019  Pan Jie

;; Author: Pan Jie <panjie@gmail.com>
;; Created: 2014-10-8
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: email tools
;; URL: https://github.com/panjie/mu4e-goodies
;; License: https://github.com/panjie/mu4e-goodies/LICENSE

;; This file is not a part of GNU Emacs.

;;; Commentary:
;;
;; Some helper functions to create shortcut in macOS or Windows to
;; quickly compose a new mail in mu4e w/wo attachments
;;
;; macOS
;; -----
;; Create a shell operation which accept files from finder in Automator
;; and install it as quick operation.
;; ---
;; files="'("
;; for f in "$@"
;; do
;;   files+="\"${f}\" "
;;   done
;; files+=")"
;; /Applications/Emacs.app/Contents/MacOS/bin/emacsclient -nc --display ns -e "(mu4e-goodies-compose-with-attachments ${files})"
;; ---
;;
;;; Code:

(require 'mu4e-utils)

;; add the most common office file's mime types to mailcap-mime-extensions
(add-to-list 'mailcap-mime-extensions '(".docx" . "application/vnd.openxmlformats-officedocument.wordprocessingml.document"))
(add-to-list 'mailcap-mime-extensions '(".doc" . "application/msword"))
(add-to-list 'mailcap-mime-extensions '(".pptx" . "application/vnd.openxmlformats-officedocument.presentationml.presentation"))
(add-to-list 'mailcap-mime-extensions '(".ppt" . "application/vnd.ms-powerpoint"))
(add-to-list 'mailcap-mime-extensions '(".xlsx" . "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"))

(defun mu4e-goodies-compose-with-attachments (&optional attachments)
  "Compose a new message with attahments."
  (mu4e-compose-new)
  (save-excursion
    (when attachments
      (dolist (file attachments)
        (if (file-exists-p file)
            (mml-attach-file
             file
             (mailcap-extension-to-mime (file-name-extension file t))
             nil
             "attachment")
          (mu4e-warn "File not found"))))))

(provide 'mu4e-goodies-compose-with-attachments)

;;; mu4e-goodies-compose-with-attachments.el ends here
