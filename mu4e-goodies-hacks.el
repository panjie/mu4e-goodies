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

;;
;; Add the following menu item to the Message menu:
;; - Insert the captured message
;; 
(when (functionp (lookup-key message-mode-map [menu-bar Message Insert\ File\ Marked...]))
  (define-key-after message-mode-map [menu-bar Message Insert\ Caputured\ Message] 
    '("Insert Captured Message" . mu4e-compose-attach-captured-message) 'Insert\ File\ Marked...))


;;
;; Allow a mu4e-view buffer detached from mu4e-header so that it will be
;; retained in a seperated window or frame.
;;
(defun mu4e-goodies-detach-msg-view (&optional towin focusnew)  
  "Detach the current mu4e-view buffer from header to a new
frame or window.

If towin is t, the detached message view will be presented in a
splitted window. Otherwise it will be presented in a new frame.

If focusnew is t, the new window/frame will be focused"
  (interactive)
  (let* ((buf (current-buffer))
         (frm (selected-frame))
         (new-win nil)
         (new-frm nil))
    (when (string= (buffer-name buf) mu4e~view-buffer-name)
      ;; rename it so that it will not be found by mu4e-view
      (rename-buffer (concat "*mu4e-view*" (mu4e-msg-field mu4e~view-msg :subject) "*") t)
      (setq mu4e~view-buffer nil)
      (if towin
          (setq new-win (split-window-below))
        (progn
          (setq new-frm (make-frame))
          (select-frame-set-input-focus frm)))
      (mu4e-view mu4e~view-msg mu4e~view-headers-buffer)
      (when focusnew
        (if towin
            (select-window new-win)
          (select-frame-set-input-focus new-frm))))))

(define-key 'mu4e-view-mode-map "\'" 'mu4e-goodies-detach-msg-view)
(define-key 'mu4e-view-mode-map "\"" (lambda () (interactive) (mu4e-goodies-detach-msg-view t nil)))

;;
;; Always put attachments to the bottom of email
;; http://mbork.pl/2015-11-28_Fixing_mml-attach-file_using_advice
;;
(defun mml-attach-file--go-to-eob (orig-fun &rest args)
  "Go to the end of buffer before attaching files."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (apply orig-fun args))))

(advice-add 'mml-attach-file :around #'mml-attach-file--go-to-eob)
(advice-add 'mml-dnd-attach-file :around #'mml-attach-file--go-to-eob)

;;
;; Quickly save last query to mu4e-bookmarks and save to custom file
;;
(defun mu4e-goodies-save-last-query-to-bookmarks (&optional notsave)
  "Save last query to mu4e-bookmarks and save to custom file"
  (interactive "P")
  (when (> (length (mu4e-last-query)) 0)
    (let ((name "")
          (key nil))
      ;; TODO: check if name and key are already exist in mu4e-bookmarks
      (while (= (length name) 0)
        (setq name (read-string "Bookmark name: ")))
      (unless (and key (= ?w (char-syntax key)))
        (setq key (read-char "Key: ")))
      (add-to-list 'mu4e-bookmarks (list (mu4e-last-query) name key) t)
      ;; save it
      (unless notsave
        (mu4e-message "Saving bookmark %s..." name)
        (customize-save-variable 'mu4e-bookmarks mu4e-bookmarks)))))

(define-key 'mu4e-headers-mode-map "K" 'mu4e-goodies-save-last-query-to-bookmarks)

;;
;; Quickly delete one email address in TO/CC/BCC fields
;;
(defun mu4e-goodies-delete-address ()
  "Quickly delete one email address in TO/CC/BCC fields"
  (interactive)
  (let ((addr-begin nil)
        (addr-end nil)
        (pos-temp nil))
    ;; Search for the beginning of the addresses, which maybe like:
    ;; - "XXX, YYY/ZZZ" <abc@def.com>
    ;; - abc@def.com
    (if (search-backward "," nil t)
        (if (re-search-backward "\"[^,]+" nil t)
            ;; "XXX, YYY/ZZZ" <abc.def.com>
            ;; ^---^
            (setq addr-begin (point))
          ;; ..., abc@def.com
          ;;    ^
          (setq addr-begin (+ 1 (point))))
      ;; To: abc@def.com
      ;; somewhere in the first of address list
      (setq addr-begin (+ 4 (search-backward "To: " nil t))))
    (goto-char addr-begin)
    (if (re-search-forward "[^@]+@[^.]+\.[^,]+," nil t)
        ;; "XXX, YYY/ZZZ" <abc.def.com>,
        ;;                             ^
        (setq addr-end (point))
      ;; at the end of address list
      (setq addr-end (line-end-position)))
    (delete-region addr-begin addr-end)))

(defun mu4e-goodies-wrapped-delete (orig-func)
  "When in address related fields, call mu4e-goodies-delete-address. 
Otherwise call orig-func which is usually \\M-d"
  (interactive)
  (let ((eoh ;; end-of-headers
        (save-excursion
          (goto-char (point-min))
          (search-forward-regexp mail-header-separator nil t))))
    (if (and eoh (> eoh (point)) (mail-abbrev-in-expansion-header-p))
        (mu4e-goodies-delete-address)
      (funcall orig-func))))

(provide 'mu4e-goodies-hacks)
