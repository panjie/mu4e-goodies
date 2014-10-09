;; Copyright (C) 2014  Pan Jie

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
