;; -*- Mode: Emacs-Lisp -*-
;;
;; elscreen-w3m.el
;;
(defconst elscreen-w3m-version "0.2.2 (July 26, 2006)")
;;
;; Author:   Naoto Morishima <naoto@morishima.net>
;; Created:  August 6, 2004
;; Revised:  July 26, 2006

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

(provide 'elscreen-w3m)
(require 'elscreen)

;; Code:

(defconst elscreen-w3m-mode-to-nickname-alist
  '(("^w3m-mode" .
     (lambda (buf)
       (with-current-buffer buf
         (let ((graphic (and window-system
                             w3m-show-graphic-icons-in-header-line)))
           (concat
            (when (and graphic w3m-use-favicon w3m-favicon-image)
              (concat
               (propertize
                " "
                'display w3m-favicon-image)
               (propertize
                " "
                'display '(space :width 0.5))))
            (w3m-current-title)))))))
  "*Alist composed of the pair of mode-name and corresponding screen-name.")
(elscreen-set-mode-to-nickname-alist 'elscreen-w3m-mode-to-nickname-alist)

(defun elscreen-w3m-mailto-url-popup-function (buffer)
  (elscreen-find-and-goto-by-buffer buffer 'create))

(defadvice w3m-copy-buffer (around elscreen-w3m-copy-buffer activate)
  (let ((current-buffer (current-buffer)))
    (if (< (elscreen-get-number-of-screens) 10)
        (elscreen-create)
      (split-window)
      (other-window 1))
    (unless (ad-get-arg 0)
      (ad-set-arg 0 current-buffer))
    ad-do-it))

(defadvice w3m-favicon-retrieve (around elscreen-w3m-favicon-retrieve activate)
  ad-do-it
  (run-at-time 1 nil 'elscreen-notify-screen-modification 'force-immediately))

(defun elscreen-w3m-initialize ()
  (setq w3m-pop-up-frames nil)
  (setq w3m-pop-up-windows nil)
  (setq w3m-use-tab nil)
  (setq w3m-use-tab-menubar nil)
  (setq w3m-use-header-line nil)
  (setq w3m-mailto-url-popup-function-alist
        '((cmail-mail-mode . elscreen-w3m-mailto-url-popup-function)
          (mail-mode . elscreen-w3m-mailto-url-popup-function)
          (message-mode . elscreen-w3m-mailto-url-popup-function)
          (mew-draft-mode . elscreen-w3m-mailto-url-popup-function)
          (mh-letter-mode . elscreen-w3m-mailto-url-popup-function)
          (wl-draft-mode . elscreen-w3m-mailto-url-popup-function)))
  (elscreen-screen-modified-hook-setup
   (w3m-fontify-after-hook 'force-immediately)))

(add-hook 'w3m-load-hook 'elscreen-w3m-initialize)
