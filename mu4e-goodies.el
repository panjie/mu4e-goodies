;;; mu4e-goodies.el --- Goodies for mu4e -*- lexical-binding: t; -*-

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
;; [mu4e](https://github.com/djcb/mu) (mu for Emacs)is a full-feature
;; email client runs inside Emacs. Here are some useful extensions/hacks
;; of mu4e.
;;
;; - Signature switch by rules
;; - Highlight the VIP's mails
;; - Check automatically if you forget to add your attachments
;; - Interaction with Lync/SkypeFB (in office)
;; - Much convenient way to manipulate tags
;; - Many useful hacks

;;; Code:

(require 'mu4e)

(defgroup mu4e-goodies nil
  "Settings for mu4e-goodies"
  :group 'mu4e)

(require 'mu4e-goodies-signature-switch)
(require 'mu4e-goodies-lync)
(require 'mu4e-goodies-actions)
(require 'mu4e-goodies-hacks)
(require 'mu4e-goodies-keyword-alert)
(require 'mu4e-goodies-tags)
(require 'mu4e-goodies-header-color)
(require 'mu4e-goodies-compose-with-attachments)

(provide 'mu4e-goodies)

;;; mu4e-goodies.el ends here
