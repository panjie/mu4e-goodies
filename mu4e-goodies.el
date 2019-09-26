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
