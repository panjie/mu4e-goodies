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


(require 'mu4e-utils)

;; add the most common office file's mime types to mailcap-mime-extensions
(add-to-list 'mailcap-mime-extensions '(".docx" . "application/vnd.openxmlformats-officedocument.wordprocessingml.document"))
(add-to-list 'mailcap-mime-extensions '(".doc" . "application/msword"))
(add-to-list 'mailcap-mime-extensions '(".pptx" . "application/vnd.openxmlformats-officedocument.presentationml.presentation"))
(add-to-list 'mailcap-mime-extensions '(".ppt" . "application/vnd.ms-powerpoint"))
(add-to-list 'mailcap-mime-extensions '(".xlsx" . "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"))

(defun mu4e-goodies-compose-with-attachments (&optional attachments)
  "Compose a new message with attahments"
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
