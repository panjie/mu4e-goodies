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

;; 
;; Change the behavior of rerun-search so that after rerun,
;; the pos will be positioned to the message before rerun is
;; run
;; 
;; NOTICE: this hack changes the behavior of some internal
;; functions of mu4e
;;
;; TODO: the current implementation is very ugly and the use of
;; defadvice will be considered in the future

(require 'mu4e)

(defvar mu4e-headers-last-docid nil "docid when running last find")
(defvar mu4e-headers-on-rerun nil "if it is in rerun")
  
(defun mu4e-headers-rerun-search ()
  "Rerun the search for the last search expression. 
NOTE: behavior of this func is change by panjie@gmail.com"
  (interactive)
  (setq mu4e-headers-last-docid (mu4e~headers-docid-at-point))
  (setq mu4e-headers-on-rerun t)
  (mu4e-headers-search mu4e~headers-last-query))

(defun mu4e~headers-found-handler (count)
  "Create a one line description of the number of headers found
after the end of the search results.
NOTE: changed by panjie@gmail.com"
  (when (buffer-live-p mu4e~headers-buffer)
    (with-current-buffer mu4e~headers-buffer
      (save-excursion
        (goto-char (point-max))
        (let ((inhibit-read-only t)
              (str (if (zerop count) mu4e~no-matches mu4e~end-of-results)))
          (insert (propertize str 'face 'mu4e-system-face 'intangible t))
          (unless (zerop count)
            (mu4e-message "Found %d matching message%s"
                          count (if (= 1 count) "" "s"))
            ;; highlight the first message
            (mu4e~headers-highlight (mu4e~headers-docid-at-point (point-min)))))
        ;; run-hooks
        (run-hooks 'mu4e-headers-found-hook)))
    (when (and mu4e-headers-on-rerun mu4e-headers-last-docid (eq (current-buffer) mu4e~headers-buffer))
      (when (mu4e~headers-goto-docid mu4e-headers-last-docid)
        (hl-line-highlight))
      (setq mu4e-headers-on-rerun nil))))

(provide 'mu4e-goodies-hacks)
