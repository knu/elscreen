;; -*- Mode: Emacs-Lisp -*-
;;
;; elscreen-howm.el
;;
(defconst elscreen-howm-version "0.1.3 (September 14, 2008)")
;;
;; Author:   Naoto Morishima <naoto@morishima.net>
;; Created:  November 06, 2005
;; Revised:  September 14, 2008

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'elscreen-howm)
(require 'elscreen)

;;; User Customizable Variables:

(defcustom elscreen-howm-mode-to-nickname-alist
  '(("^howm-\\(menu-mode$\\|view-\\)" . "howm(menu)"))
  "*Alist composed of the pair of mode-name and corresponding screen-name."
  :type '(alist :key-type string :value-type (choice string function))
  :tag "Howm major-mode to nickname alist"
  :set (lambda (symbol value)
         (custom-set-default symbol value)
         (elscreen-rebuild-mode-to-nickname-alist))
  :group 'howm)
(elscreen-set-mode-to-nickname-alist 'elscreen-howm-mode-to-nickname-alist)

(defcustom elscreen-howm-buffer-to-nickname-alist
  '(("\\.howm$" . elscreen-howm-mode-buffer-to-nickname))
  "*Alist composed of the pair of regular expression of
buffer-name and corresponding screen-name."
  :type '(alist :key-type string :value-type (choice string function))
  :tag "Howm buffer to nickname alist"
  :set (lambda (symbol value)
         (custom-set-default symbol value)
         (elscreen-rebuild-buffer-to-nickname-alist))
  :group 'howm)
(elscreen-set-buffer-to-nickname-alist 'elscreen-howm-buffer-to-nickname-alist)


;;; Code:

;; delete other windows when howm-menu is invoked
(defadvice howm-menu (around elscreen-howm-menu activate)
  (delete-other-windows)
  ad-do-it)

;; create new page with new screen
(defadvice howm-create-file-with-title (around elscreen-howm-create-file-with-title activate)
  (save-current-buffer
    (elscreen-create))
  ad-do-it)

;; open existing page with new screen
(defadvice howm-view-open-item (around elscreen-howm-view-open-item activate)
  (let ((window-configuration (elscreen-current-window-configuration))
        howm-item-buffer)
    ad-do-it
    (setq howm-item-buffer (current-buffer))
    (elscreen-apply-window-configuration window-configuration)
    (elscreen-find-and-goto-by-buffer howm-item-buffer 'create)))

(eval-after-load "howm-view"
  '(unless (fboundp 'howm-view-open-item)
     (elscreen-message "This version of elscreen-howm does not work well with your howm.  Upgrade howm to version 1.3.3 or later, or downgrade elscreen-howm to version 0.0.1.")))

;; screen nickname
(defun elscreen-howm-mode-buffer-to-nickname ()
  (format "howm(%s%s%s)"
          elscreen-howm-title
          (if (zerop (length elscreen-howm-title)) "" ":")
          (substring (buffer-name (current-buffer)) 0 -5)))

(defun elscreen-howm-mode-update-title ()
  (let ((title (howm-title-at-current-point)))
    (when (not (string-equal elscreen-howm-title title))
      (setq elscreen-howm-title title)
      (elscreen-notify-screen-modification 'force-immediately))))

(defun elscreen-howm-mode-initialize ()
  (set (make-local-variable 'elscreen-howm-title)
       (howm-title-at-current-point))
  (add-hook 'post-command-hook 'elscreen-howm-mode-update-title t t))

(add-hook 'howm-mode-on-hook 'elscreen-howm-mode-initialize)

;; kill screen when exiting from howm-mode
(defun howm-save-and-kill-buffer/screen ()
  (interactive)
  (let* ((file-name (buffer-file-name)))
    (if (save-excursion
          (goto-char (point-min))
          (re-search-forward "[^ \t\r\n]" nil t))
        (howm-save-buffer)
      (set-buffer-modified-p nil)
      (when (file-exists-p file-name)
        (delete-file file-name)
        (message "(Deleted %s)" (file-name-nondirectory file-name))))
    (kill-buffer nil)
    (unless (elscreen-one-screen-p)
      (elscreen-kill))))

(eval-after-load "howm-mode"
  '(progn
     (define-key howm-mode-map
       "\C-c\C-c" 'howm-save-and-kill-buffer/screen)))
