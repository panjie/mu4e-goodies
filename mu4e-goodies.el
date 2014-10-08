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
