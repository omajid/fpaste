;;; fpaste.el --- upload selections to fpaste.org

;;; Copyright (C) 2014 Omair Majid

;; Author: Omair Majid <omair.majid@gmail.com>
;; URL: http://github.com/omajid/fpaste
;; Keywords: convenience tools
;; Version: 0.0.1.20140916
;; Package-Requires:

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;;   Use the command line `fpaste` program to upload pastes to the
;;   Fedora pastebin.

;;; Code:

(defconst fpaste-program "fpaste"
  "The program to use to upload pastes.")

(defun fpaste-current-buffer ()
  "Upload the contents of this buffer into fpaste."
  (interactive)
  (fpaste--selection (point-min) (point-max)))

(defun fpaste-current-selection ()
  "Upload the current selection into fpaste."
  (interactive)
  (fpaste--selection (region-beginning) (region-end)))

;; FIXME open browser with the paste

(defun fpaste--selection (beg end)
  "Upload the region defined by BEG and END into fpaste."
  (if (executable-find fpaste-program)
      (let* ((result (shell-command-on-region beg end fpaste-program))
             (output (with-current-buffer "*Shell Command Output*"
                         (buffer-substring (point-min) (point-max)))))
        (progn
          (string-match "http://.*fedoraproject.*$" output)
          (let ((url (match-string 0 output)))
            (message url))))
    (message "'%s' is not available. Please install it." fpaste-program)))

(provide 'fpaste)
;;; fpaste.el ends here