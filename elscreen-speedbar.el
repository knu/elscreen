;; -*- Mode: Emacs-Lisp -*-
;;
;; elscreen-speedbar.el
;;
(defconst elscreen-speedbar-version "0.0.0 (November 18, 2007)")
;;
;; Author:   Naoto Morishima <naoto@morishima.net>
;; Created:  November 18, 2007

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

(provide 'elscreen-speedbar)
(require 'elscreen)

(defcustom elscreen-speedbar-find-file-in-screen t
  "Non-nil to use ElScreen to open file when the selected file is being
opened in the attached frame."
  :type 'boolean
  :group 'speedbar)

(defadvice speedbar-frame-mode (after elscreen-speedbar-frame-mode activate)
  (with-current-buffer speedbar-buffer
    (set (make-local-variable 'elscreen-display-tab) nil)))

(defadvice speedbar-find-file-in-frame (around elscreen-speedbar-find-file-in-frame activate)
  (let ((buffer (find-file-noselect file)))
    (if (or (get-buffer-window buffer 0)
            dframe-power-click
            (numberp speedbar-select-frame-method)
            (not elscreen-speedbar-find-file-in-screen))
        (let ((dframe-power-click (and (not elscreen-speedbar-find-file-in-screen)
                                       dframe-power-click)))
          ad-do-it)
      (dframe-select-attached-frame speedbar-frame)
      (elscreen-find-and-goto-by-buffer buffer 'create))
    (elscreen-notify-screen-modification 'force-immediately)))
