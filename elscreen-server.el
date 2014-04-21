;;; elscreen-server.el --- server support for elscreen
;;
(defconst elscreen-server-version "0.2.0 (November 23, 2007)")
;;
;; Author:   Hideyuki Shirai <shirai@meadowy.org>
;;           Naoto Morishima <naoto@morishima.net>
;; Created:  October 11, 2007
;; Revised:  November 23, 2007

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

;;; Commentary:

;;; Code:

(require 'elscreen)
(require 'dframe)

(defcustom elscreen-server-dont-use-dedicated-frame t
  "*Non-nil to use dframe-attached-frame if frame is dedicated."
  :type 'boolean
  :group 'server)

(defun elscreen-server-visit-files-new-screen (buffer-list)
  "Create a screen for each buffer in BUFFER-LIST."
  (let* ((selected-frame (selected-frame))
         (dframe-attached-frame (dframe-attached-frame selected-frame)))
    (when (and elscreen-server-dont-use-dedicated-frame
               (framep dframe-attached-frame))
      (select-frame dframe-attached-frame))
    (let ((screen (car (mapcar
                        (lambda (buffer)
                          (elscreen-find-screen-by-buffer buffer 'create))
                        buffer-list))))
      (and screen
           (elscreen-goto screen)))
    (elscreen-notify-screen-modification 'force-immediately)
    (select-frame selected-frame)))

(eval-after-load "server"
  ;; For server.el distributed with GNU Emacs
  '(progn
     (defadvice server-visit-files (after elscreen-server-visit-files activate)
       (elscreen-server-visit-files-new-screen ad-return-value))))

(provide 'elscreen-server)
;;; elscreen-server.el ends here
