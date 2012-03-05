;; -*- Mode: Emacs-Lisp -*-
;;
;; elscreen-color-theme.el
;;
(defconst elscreen-color-theme-version "0.0.0 (November 19, 2007)")
;;
;; Author:   Naoto Morishima <naoto@morishima.net>
;; Created:  November 19, 2007

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

(provide 'elscreen-color-theme)
(require 'elscreen)

(defcustom elscreen-color-theme-override-theme nil
  "Non-nil to override theme's faces"
  :type 'boolean
  :group 'color-theme)

(defcustom elscreen-color-theme-tab-background-face-function
  'elscreen-color-theme-tab-background-face-default-function
  "Function to generate a face for background of the tabs of ElScreen."
  :type 'function
  :group 'color-theme)

(defcustom elscreen-color-theme-tab-control-face-function
  'elscreen-color-theme-tab-control-face-default-function
  "Function to generate a face for the control tab of ElScreen."
  :type 'function
  :group 'color-theme)

(defcustom elscreen-color-theme-tab-current-screen-face-function
  'elscreen-color-theme-tab-current-screen-face-default-function
  "Function to generate a face for the current tab of ElScreen."
  :type 'function
  :group 'color-theme)

(defcustom elscreen-color-theme-tab-other-screen-face-function
  'elscreen-color-theme-tab-other-screen-face-default-function
  "Function to generate a face for inactive tabs of ElScreen."
  :type 'function
  :group 'color-theme)

(defsubst elscreen-color-theme-generate-color (color weight)
  (let* ((max-value (car (color-values "white")))
         (dividing-value (round (/ max-value 2)))
         (unit-value (round (/ dividing-value 16))))
    (apply 'format "#%02x%02x%02x"
           (mapcar
            (lambda (value)
              (let* ((sign (if (< dividing-value value) -1 1))
                     (adjustment (* sign unit-value weight)))
                (+ value adjustment)))
            (color-values color)))))

(defun elscreen-color-theme-tab-background-face-default-function (theme)
  (let* ((params (color-theme-frame-params theme))
         (background (cdr (assoc 'background-color params)))
         (faces
          (when background
            `(:background
              ,(elscreen-color-theme-generate-color background 8)))))
    (when faces `((t ,faces)))))

(defun elscreen-color-theme-tab-control-face-default-function (theme)
  (let* ((params (color-theme-frame-params theme))
         (foreground (cdr (assoc 'foreground-color params)))
         (background (cdr (assoc 'background-color params)))
         (faces (nconc
                 (when foreground `(:foreground ,foreground))
                 (when background `(:background ,background)))))
    (when faces `((t ,faces)))))

(defalias 'elscreen-color-theme-tab-current-screen-face-default-function
  'elscreen-color-theme-tab-control-face-default-function)

(defun elscreen-color-theme-tab-other-screen-face-default-function (theme)
  (let* ((params (color-theme-frame-params theme))
         (foreground (cdr (assoc 'foreground-color params)))
         (background (cdr (assoc 'background-color params)))
         (faces (nconc
                 (when foreground
                   `(:foreground
                     ,(elscreen-color-theme-generate-color foreground 12)))
                 (when background
                   `(:background
                     ,(elscreen-color-theme-generate-color background 4))))))
    (when faces `((t ,faces)))))

(defadvice color-theme-install (around elscreen-color-theme-install activate)
  (let* ((theme-faces (color-theme-faces (color-theme-canonic theme)))
         (elscreen-faces
          (delete nil
                  (mapcar
                   (lambda (face-name)
                     (unless (and (not elscreen-color-theme-override-theme)
                                  (assoc face-name theme-faces))
                       (let* ((face-fn
                               (symbol-value
                                (intern
                                 (concat (replace-regexp-in-string
                                          "^elscreen"
                                          "elscreen-color-theme"
                                          (symbol-name face-name))
                                         "-function"))))
                              (faces (funcall face-fn theme)))
                         (when faces (list face-name faces)))))
                   '(elscreen-tab-background-face
                     elscreen-tab-control-face
                     elscreen-tab-current-screen-face
                     elscreen-tab-other-screen-face)))))
    ad-do-it
    (when elscreen-faces
      (color-theme-install-faces elscreen-faces))))
