;; -*- Mode: Emacs-Lisp -*-
;;
;; elscreen-dnd.el
;;
(defconst elscreen-dnd-version "0.0.0 (December 15, 2005)")
;;
;; Author:   Hideyuki Shirai <shirai@meadowy.org>
;;           Naoto Morishima <naoto@morishima.net>
;; Created:  December 15, 2005

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

(provide 'elscreen-dnd)
(require 'elscreen)

(defcustom elscreen-dnd-open-file-new-screen t
  "If non-nil, always create new screen to open dropped files."
  :type 'boolean
  :group 'dnd)


;; Code:

(defmacro elscreen-dnd-drag-n-drop (ad-do-it)
  (` (progn
       (elscreen-notify-screen-modification-suppress
	(, ad-do-it))
       (elscreen-notify-screen-modification 'force))))

(defadvice dnd-handle-one-url (around elscreen-dnd-handle-open-url activate)
  (if (not elscreen-dnd-open-file-new-screen)
      ad-do-it
    (let ((dnd-open-file-other-window nil)
	  file-buffer)
      (save-window-excursion
	ad-do-it
	(setq file-buffer (current-buffer)))
      (if (elscreen-screen-modified-p 'dnd-handle-one-url)
	  (elscreen-find-and-goto-by-buffer file-buffer 'create)
	(elscreen-find-screen-by-buffer file-buffer 'create)))))

(mapc
 (lambda (drag-n-drop-function)
   (eval (` (defadvice (, drag-n-drop-function) (around elscreen-dnd-drag-n-drop activate)
	      (elscreen-dnd-drag-n-drop ad-do-it)))))
 (list 'x-dnd-handle-drag-n-drop-event 'mac-drag-n-drop 'w32-drag-n-drop))
