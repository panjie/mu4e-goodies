;;; mu4e-goodies-utils.el --- Some common functions  -*- lexical-binding: t; -*-

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

;;
;; some utility functions
;;

(defsubst mu4e-goodies~get-real-addr (addr)
  "Parse addr which is the result of mu4e-message-fields to get
the real email address"
  (if (listp addr)       ;; already parsed by mu4e
      (cdr (car addr))
    (if (stringp addr)   ;; raw address like: "ABC <abc@abc.com>"
        (car (mail-header-parse-address addr)))))


(defvar mu4e-goodies~header-handlers nil
  "Internal handlers of header view for mu >= 1.5")


(defun mu4e-goodies~header-advice (orig-func &rest args)
  "General advice for plugins for header view"
  (let* ((str (apply orig-func args))
         (msg (car args))
         (field (cadr args)))
    (dolist (func mu4e-goodies~header-handlers)
      (setq str (funcall func msg field (mu4e-message-field msg field) str)))
    str))

(when (functionp 'mu4e~headers-field-value) ; mu >= 1.5
  (advice-add 'mu4e~headers-field-value :around #'mu4e-goodies~header-advice))

(provide 'mu4e-goodies-utils)

;;; mu4e-goodies-utils.el ends here
