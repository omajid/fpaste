;;; fpaste.el --- upload selections to fpaste.org

;;; Copyright (C) 2014 Omair Majid

;; Author: Omair Majid <omair.majid@gmail.com>
;; URL: http://github.com/omajid/fpaste
;; Keywords: convenience tools
;; Version: 0.0.1.20140924
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

;; Use the command line `fpaste` program to upload pastes to the
;; Fedora pastebin.
;;
;; Use the `fpaste-current-region-or-buffer' function which will
;; upload the right thing (the current selection, if any, or the
;; buffer) to fpaste.


;;; Code:

(defgroup fpaste nil
  "Group for fpaste-related customization.")

(defcustom fpaste-program "fpaste"
  "The program to use to upload pastes to the Fedora pastebin."
  :group 'fpaste)

(defcustom fpaste-output-url-regex "\\(http://[^ ]*fedoraproject.*\\)$"
  "A regexp that matches the URL of the paste.

The first match should be the URL."
  :group 'fpaste)

;;;###autoload
(defun fpaste-current-region-or-buffer ()
  "Upload the contents of the current buffer or region to fpaste."
  (interactive)
  (if (use-region-p)
      (fpaste--region (region-beginning) (region-end))
    (fpaste--region (point-min) (point-max))))

(defun fpaste--region (beg end)
  "Upload the region defined by BEG and END into fpaste."
  (if (executable-find fpaste-program)
      (let* ((result (shell-command-on-region beg end fpaste-program))
             (output (with-current-buffer "*Shell Command Output*"
                         (buffer-substring (point-min) (point-max)))))
        (progn
          (string-match fpaste-output-url-regex output)
          (let ((url (match-string 1 output)))
            (message url)
            (browse-url url))))
    (message "'%s' is not available. Please install it." fpaste-program)))

(provide 'fpaste)
;;; fpaste.el ends here
