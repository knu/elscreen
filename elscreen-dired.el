;; -*- Mode: Emacs-Lisp -*-
;;
;; elscreen-dired.el 
;;
(defconst elscreen-dired-version "0.1.0 (November 6, 2005)")
;;
;; Author:   Naoto Morishima <naoto@morishima.net>
;; Created:  August 9, 2004
;; Revised:  November 6, 2005

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

(provide 'elscreen-dired)
(require 'elscreen)


;;; Code:

(defadvice dired-find-file-other-window (around elscreen-dired-find-file-other-window activate)
  (let ((window-configuration (current-window-configuration))
	(buffer nil))
    ad-do-it
    (unless (eq major-mode 'dired-mode)
      (setq buffer (current-buffer))
      (set-window-configuration window-configuration)
      (elscreen-find-and-goto-by-buffer buffer t))))
