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

;; Add indicators to mode-line to show the status of 3 variables:
;; - mu4e-headers-full-search
;; - mu4e-headers-show-threads: the swither when you push P
;; - mu4e-headers-include-related: the swither when you push W
;;============================================================
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


(provide 'mu4e-goodies-indicators)
