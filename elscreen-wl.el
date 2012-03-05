;; -*- Mode: Emacs-Lisp -*-
;;
;; elscreen-wl.el
;;
(defconst elscreen-wl-version "0.8.0 (November 03, 2007)")
;;
;; Author:   Naoto Morishima <naoto@morishima.net>
;; Created:  March 24, 2001
;; Revised:  November 03, 2007

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

(provide 'elscreen-wl)
(require 'elscreen)
(require 'wl)


;;; User Customizable Variables:

(defcustom elscreen-wl-draft-use-elscreen t
  "Non-nil to use ElScreen when Wanderlust prepares draft buffer.
This option will override the option `wl-draft-use-frame'."
  :type 'boolean
  :group 'wl-draft)

(defcustom elscreen-wl-display-biff-on-tab t
  "Non-nil to use ElScreen's tab to display biff icon instead of
mode-line."
  :type 'boolean
  :set (lambda (symbol value)
         (custom-set-default symbol value)
         (let ((buf (get-buffer wl-folder-buffer-name)))
           (when buf
             (with-current-buffer buf
               (wl-mode-line-buffer-identification))))
         (elscreen-notify-screen-modification 'force-immediately))
  :group 'wl-highlight)

(defcustom elscreen-wl-mode-to-nickname-alist
  '(("^wl-folder-mode$" . (lambda (buf)
                            (elscreen-wl-nickname-with-biff-status "Folder")))
    ("^wl-summary-mode$" . (lambda (buf)
                             (elscreen-wl-nickname-with-biff-status
                              (wl-summary-buffer-folder-name))))
    ("^wl-draft-mode$" . (lambda (buf)
                           (let ((buffer-name (buffer-name))
                                 (subject (std11-field-body "Subject")))
                             (format "WL(%s%s)"
                                     buffer-name
                                     (if (zerop (length subject))
                                         ""
                                       (concat "[" subject "]"))))))
    ("^wl-" . "Wanderlust"))
  "*Alist composed of the pair of mode-name and corresponding screen-name."
  :type '(alist :key-type string :value-type (choice string function))
  :tag "Wanderlust major-mode to screen nickname alist"
  :set (lambda (symbol value)
         (custom-set-default symbol value)
         (elscreen-rebuild-mode-to-nickname-alist))
  :group 'wl)
(elscreen-set-mode-to-nickname-alist 'elscreen-wl-mode-to-nickname-alist)


;;; Code:

(defmacro elscreen-wl-draft-create-buffer (ad-do-it)
  `(let ((wl-draft-use-frame wl-draft-use-frame)
         (wl-draft-reply-buffer-style wl-draft-reply-buffer-style))
     (when elscreen-wl-draft-use-elscreen
       (setq wl-draft-use-frame nil)
       (setq wl-draft-reply-buffer-style 'full)
       (elscreen-create))
     ,ad-do-it
     (when elscreen-wl-draft-use-elscreen
       (make-local-variable 'kill-buffer-hook)
       (add-hook 'kill-buffer-hook
                 (lambda ()
                   (when `(buffer-live-p ,(current-buffer))
                     (elscreen-kill)))))))

(defadvice wl-draft-create-buffer (around
                                   elscreen-ad-wl-draft-create-buffer
                                   activate)
  (elscreen-wl-draft-create-buffer ad-do-it))

(defadvice wl-draft-reedit (around
                            elscreen-ad-wl-draft-reedit
                            activate)
  (elscreen-wl-draft-create-buffer ad-do-it))

(defadvice wl-jump-to-draft-buffer (around
                                    elscreen-ad-wl-jump-to-draft-screen
                                    activate)
  (let ((original-buffer (current-buffer))
        draft-buffer)
    ad-do-it
    (when elscreen-wl-draft-use-elscreen
      (setq draft-buffer (current-buffer))
      (if (not (eq original-buffer draft-buffer))
          (progn
            (switch-to-buffer original-buffer)
            (elscreen-find-and-goto-by-buffer draft-buffer))))))

(defmacro elscreen-wl-compensate-summary-window (ad-do-it)
  `(let ((wl-draft-use-frame (or wl-draft-use-frame
                                 elscreen-wl-draft-use-elscreen)))
     ,ad-do-it))

(defadvice wl-draft-forward (around elscreen-ad-wl-draft-forward activate)
  (elscreen-wl-compensate-summary-window ad-do-it))

(defadvice wl-draft-reply (around elscreen-ad-wl-draft-forward activate)
  (elscreen-wl-compensate-summary-window ad-do-it))

;; Biff on ElScreen's tab

(defsubst elscreen-wl-nickname-with-biff-status (name)
  (concat
   (when (and wl-biff-check-folder-list elscreen-wl-display-biff-on-tab)
     (concat
      (if wl-modeline-biff-status
          wl-modeline-biff-state-on
        wl-modeline-biff-state-off)
      (propertize " " 'display '(space :width 0.5))))
   (format "WL(%s)" name)))

(add-hook 'wl-biff-notify-hook
          (lambda ()
            (when elscreen-wl-display-biff-on-tab
              (setq wl-modeline-biff-status t)
              (elscreen-notify-screen-modification 'force-immediately))))

(add-hook 'wl-biff-unnotify-hook
          (lambda ()
            (when elscreen-wl-display-biff-on-tab
              (setq wl-modeline-biff-status nil)
              (elscreen-notify-screen-modification 'force-immediately))))

(defadvice wl-mode-line-buffer-identification (around elscreen-wl-mode-line-buffer-identification activate)
  (let ((wl-biff-check-folder-list wl-biff-check-folder-list))
    (when elscreen-wl-display-biff-on-tab
      (setq wl-biff-check-folder-list nil))
    ad-do-it))
