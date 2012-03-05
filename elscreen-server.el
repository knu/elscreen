;; -*- Mode: Emacs-Lisp -*-
;;
;; elscreen-server.el
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

(provide 'elscreen-server)
(require 'elscreen)

(defmacro elscreen-server-defcustom-dont-use-dedicated-frame (type)
  `(defcustom elscreen-server-dont-use-dedicated-frame t
     "*Non-nil to use dframe-attached-frame if frame is dedicated"
     :type 'boolean
     :group ,type))

(defsubst elscreen-server-visit-files-new-screen (buffer-list)
  (let* ((selected-frame (selected-frame))
         (dframe-attached-frame (and (fboundp 'dframe-attached-frame)
                                     (dframe-attached-frame selected-frame))))
    (when (and elscreen-server-dont-use-dedicated-frame
               (framep dframe-attached-frame))
      (select-frame dframe-attached-frame))
    (elscreen-goto (car (mapcar
                         (lambda (buffer)
                           (elscreen-find-screen-by-buffer buffer 'create))
                         buffer-list)))
    (elscreen-notify-screen-modification 'force-immediately)
    (select-frame selected-frame)))

(eval-after-load "server"
  ;; For server.el distributed with GNU Emacs
  '(progn
     (elscreen-server-defcustom-dont-use-dedicated-frame 'server)

     (defadvice server-visit-files (after elscreen-server-visit-files activate)
       (elscreen-server-visit-files-new-screen
        (if (processp (car ad-return-value))
            ;; Before multi-tty; server-visit-files returns a list of proc
            ;; and client-record.
            (cdr ad-return-value)
          ;; After multi-tty was merged; in server.el r1.131 or later, it
          ;; returns only client-record.
          ad-return-value)))))

(eval-after-load "gnuserv"
  '(progn
     (elscreen-server-defcustom-dont-use-dedicated-frame 'gnuserv)

     (defun elscreen-server-find-buffer-visiting (filename)
       (if (file-directory-p filename)
           (car (dired-buffers-for-dir filename))
         (find-buffer-visiting filename)))

     (cond
      ((fboundp 'gnuserv-edit-files)
       ;; For (current) gnuserv typically used with XEmacs
       (defadvice gnuserv-edit-files (around elscreen-gnuserv-edit-files activate)
         (let ((filename-list (mapcar 'cdr list))
               (gnuserv-frame t))
           (save-window-excursion ad-do-it)
           (elscreen-server-visit-files-new-screen
            (mapcar 'elscreen-server-find-buffer-visiting filename-list)))))
      ((fboundp 'server-find-file)
       ;; For (ancient) gnuserv typically used with Meadow
       (defadvice server-edit-files (around elscreen-server-edit-files activate)
         (let ((filename-list (mapcar 'cdr list))
               (gnuserv-frame (selected-frame)))
           (save-window-excursion ad-do-it)
           (elscreen-server-visit-files-new-screen
            (mapcar 'elscreen-server-find-buffer-visiting filename-list))))
       (defadvice server-edit-files-quickly (around elscreen-server-edit-files-quickly activate)
         (let ((filename-list (mapcar 'cdr list))
               (gnuserv-frame (selected-frame)))
           (save-window-excursion ad-do-it)
           (elscreen-server-visit-files-new-screen
            (mapcar 'elscreen-server-find-buffer-visiting filename-list))))))))
