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
(require 'browse-url)


(defun mu4e-goodies-lync-chat (&optional emails)
  "Lync with the email address at point if parameter email is not given.

Note:
- It will only work under windows/cygwin/macOS
- If used under cygwin, you should prepare a script named \"lya.bat\" like the following:
  -----------------------------------------
  @echo off
  setlocal enableDelayedExpansion

  set SIPS=
  :LOOP
        if [%1] == [] goto END
        set \"TEMPSIP=<sip:%1>\"
        set SIPS=!SIPS!!TEMPSIP!
        shift
        goto LOOP
  :END

  IF NOT !SIPS! == \"\" start im:!SIPS!
  ----------------------------------------
- If used under macOS, SkypeFB is required."
  
  (interactive)
  (if (and (listp emails) (> (length emails) 0))
      ;; an email list
      (cond ((eq system-type 'windows-nt)
             (browse-url (let ((uri "im:"))
                           (dolist (email emails)
                             (unless (string= email user-mail-address) 
                               (setq uri (concat uri "<sip:" email ">"))))
                           (mu4e-message "Lync with %s" uri)
                           uri)))
            ((eq system-type 'cygwin)
             (let ((str ""))
               (dolist (email emails)
                 (unless (string= email user-mail-address)
                   (setq str (concat str " " email))))
               (shell-command (concat "lya.bat " str)))))
    (if (stringp emails)
        ;; a single email
        (browse-url (concat "sip:" emails))
      ;; get email address from point
      (browse-url (concat "sip:" (get-text-property (point) 'email))))))


(define-key mu4e-view-contacts-header-keymap "L" 'mu4e-goodies-lync-chat)

(provide 'mu4e-goodies-lync)
