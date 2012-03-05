;; -*- Mode: Emacs-Lisp -*-
;;
;; elscreen-goby.el 
;;
(defconst elscreen-goby-version "0.0.0 (July 14, 2005)")
;;
;; Author:   Naoto Morishima <naoto@dl.naist.jp>
;;              Nara Institute of Science and Technology, Japan
;; Created:  July 14, 2005

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

(provide 'elscreen-goby)
(require 'elscreen)


;;; Code:

(defadvice goby-view-mode (before elscreen-goby-view-mode activate)
  (set (make-local-variable 'elscreen-display-tab) nil))

(defadvice goby-view-quit (before elscreen-goby-view-quit activate)
  (kill-local-variable 'elscreen-display-tab))
