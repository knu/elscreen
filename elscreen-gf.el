;; -*- Mode: Emacs-Lisp -*-
;;
;; elscreen-gf.el
;;
(defconst elscreen-gf-version "1.5.3 (November 07, 2007)")
;;
;; Author:   Naoto Morishima <naoto@morishima.net>
;; Based on: grep-family.el
;;              by Youki Kadobayashi <youki-k@is.aist-nara.ac.jp>
;; Created:  June 23, 1996
;; Revised:  November 07, 2007

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

(provide 'elscreen-gf)
(require 'elscreen)
(require 'poe)
(eval-when-compile
  (require 'static))

;;; User Customizable Variables:

(defgroup elscreen-gf nil
  "ElScreen-GF -- Grep Family on ElScreen"
  :tag "ElScreen-GF"
  :group 'tools)

(defcustom elscreen-gf-grep-program-name "grep"
  "Program invoked by \\[elscreen-gf-grep] command."
  :type 'string
  :group 'elscreen-gf)

(defcustom elscreen-gf-idutils-gid-program-name "gid"
  "Program invoked by \\[elscreen-gf-idutils-gid] command."
  :type 'string
  :group 'elscreen-gf)

(defcustom elscreen-gf-idutils-mkid-program-name "mkid"
  "Program invoked by \\[elscreen-gf-idutils-mkid] command."
  :type 'string
  :group 'elscreen-gf)

(defcustom elscreen-gf-cscope-program-name "cscope"
  "Program invoked by \\[elscreen-gf-cscope] command."
  :type 'string
  :group 'elscreen-gf)

(defcustom elscreen-gf-global-program-name "global"
  "Program invoked by \\[elscreen-gf-global] command."
  :type 'string
  :group 'elscreen-gf)

(defcustom elscreen-gf-global-gtags-program-name "gtags"
  "Program invoked by \\[elscreen-gf-global-gtags] command."
  :type 'string
  :group 'elscreen-gf)

(defcustom elscreen-gf-mode-truncate-lines t
  "If non-nil, truncate each line in ElScreen-GF mode."
  :tag "Truncate lines in ElScreen-GF mode."
  :type 'boolean
  :group 'elscreen-gf)

(defcustom elscreen-gf-invoke-point-history-length 8
  "Length of histories for the locations where gf searchs were invoked."
  :tag "Length of histries for invoke-point."
  :type '(integer :size 4)
  :set (lambda (symbol value)
         (when (and (numberp value)
                    (>= value 4)
                    (<= value 64))
           (custom-set-default symbol value)
           (let ((old-history (when (boundp 'elscreen-gf-invoke-point-history)
                                elscreen-gf-invoke-point-history)))
             (setq elscreen-gf-invoke-point-history (make-ring value))
             (when (and old-history (ring-p old-history))
               (while (and (not (ring-empty-p old-history))
                           (< (ring-length elscreen-gf-invoke-point-history)
                              value))
                 (ring-insert-at-beginning elscreen-gf-invoke-point-history
                                           (ring-remove old-history 0)))))))
  :group 'elscreen-gf)

(defface elscreen-gf-mode-selected-entry-face
  '((t (:underline t)))
  "Face used for the selected entry in ElScreen-GF mode."
  :group 'elscreen-gf)

(defface elscreen-gf-mode-file-name-face
  '((((class color) (background dark))
     (:foreground "khaki" :bold t))
    (((class color) (background light))
     (:foreground "DarkSlateBlue" :bold t))
    (t (:bold t)))
  "Face used for file name in ElScreen-GF mode."
  :group 'elscreen-gf)

(defface elscreen-gf-mode-line-number-face
  '((((class color) (background dark))
     (:foreground "gray" :bold t))
    (((class color) (background light))
     (:foreground "gray50" :bold t))
    (t (:bold t)))
  "Face used for line number in ElScreen-GF mode."
  :group 'elscreen-gf)

(defface elscreen-gf-mode-pattern-face
  '((((class color) (background dark))
     (:foreground "tomato" :bold t))
    (((class color) (background light))
     (:foreground "tomato" :bold t))
    (t (:bold t)))
  "Face used to emphasize the specified keyword in ElScreen-GF mode."
  :group 'elscreen-gf)

(defface elscreen-gf-emphasis-after-jump-face
  '((((class color) (background light))
     (:background "LightSteelBlue1"))
    (((class color) (background dark))
     (:background "SteelBlue"))
    (t (:bold t)))
  "Face used to emphasize the selected line after jump."
  :group 'elscreen-gf)

;;; Key bindings:

(defvar elscreen-gf-map (make-sparse-keymap)
  "Keymap for elscreen-gf.")
(define-key elscreen-gf-map "G" 'elscreen-gf-grep)
(define-key elscreen-gf-map "m" 'elscreen-gf-idutils-mkid)
(define-key elscreen-gf-map "g" 'elscreen-gf-idutils-gid)
(define-key elscreen-gf-map "c" 'elscreen-gf-cscope)
(define-key elscreen-gf-map "t" 'elscreen-gf-global-gtags)
(define-key elscreen-gf-map "l" 'elscreen-gf-global)
(define-key elscreen-gf-map "u" 'elscreen-gf-go-back-to-latest-invoke-point)
(define-key elscreen-gf-map "v" 'elscreen-gf-display-version)

(define-key elscreen-map "\C-g" elscreen-gf-map)

(defvar elscreen-gf-mode-map (make-sparse-keymap)
  "keymap used in elscreen-gf mode.")
(define-key elscreen-gf-mode-map "n"    'elscreen-gf-mode-next-line)
(define-key elscreen-gf-mode-map "p"    'elscreen-gf-mode-previous-line)
(define-key elscreen-gf-mode-map " "    'elscreen-gf-mode-scroll-up)
(define-key elscreen-gf-mode-map "\177" 'elscreen-gf-mode-scroll-down)
(define-key elscreen-gf-mode-map "<"    'elscreen-gf-mode-beginning-of-buffer)
(define-key elscreen-gf-mode-map ">"    'elscreen-gf-mode-end-of-buffer)
(define-key elscreen-gf-mode-map "N"    'elscreen-gf-mode-next-file)
(define-key elscreen-gf-mode-map "P"    'elscreen-gf-mode-previous-file)
(define-key elscreen-gf-mode-map "t"    'elscreen-gf-mode-truncate-lines-toggle)
(define-key elscreen-gf-mode-map "o"    'elscreen-gf-mode-jump-to-entry)
(define-key elscreen-gf-mode-map "O"    'elscreen-gf-mode-jump-to-entry-read-only)
(define-key elscreen-gf-mode-map "\C-g" 'elscreen-gf-mode-search-quit)
(define-key elscreen-gf-mode-map "q"    'elscreen-gf-mode-exit)
(define-key elscreen-gf-mode-map "v"    'elscreen-gf-display-version)


;;; Internal Functions and Variables:

(defvar elscreen-gf-invoke-point-history
  (make-ring elscreen-gf-invoke-point-history-length))

(eval-when-compile
  (defun-maybe line-number-at-pos (&optional pos)
    (let ((opoint (or pos (point))))
      (save-excursion
        (goto-char opoint)
        (forward-line 0)
        (1+ (count-lines (point-min) (point)))))))

(defsubst elscreen-gf-overlay-create (start end face)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face face)
    (overlay-put overlay 'evaporate t)
    overlay))

(defsubst elscreen-gf-move-overlay-create (overlay-symbol start end face)
  (let ((overlay (condition-case nil
                     (symbol-value overlay-symbol)
                   (error nil))))
    (if (overlayp overlay)
        (move-overlay overlay start end (current-buffer))
      (set overlay-symbol (elscreen-gf-overlay-create start end face))
      (setq overlay (symbol-value overlay-symbol)))
    overlay))

(defun elscreen-gf-define-major-mode-token (major-mode token-chars)
  (let ((thing-symbol (intern (format "%s-thing" (symbol-name major-mode)))))
    (put major-mode 'elscreen-gf-major-mode-thing thing-symbol)
    (put thing-symbol 'token-chars token-chars)
    (put thing-symbol 'end-op
         `(lambda ()
            (re-search-forward (concat "\\=[" ,token-chars "]*") nil t)))
    (put thing-symbol 'beginning-op
         `(lambda ()
            (if (re-search-backward (concat "[^" ,token-chars "]") nil t)
                (forward-char)
              (goto-char (point-min)))))))

(elscreen-gf-define-major-mode-token 'c-mode "a-zA-Z0-9_")
(elscreen-gf-define-major-mode-token 'perl-mode "a-zA-Z0-9_")
(elscreen-gf-define-major-mode-token 'emacs-lisp-mode "-a-zA-Z0-9/+:<>")

(defsubst elscreen-gf-major-mode-token-chars (major-mode)
  (get (get major-mode 'elscreen-gf-major-mode-thing) 'token-chars))

(defsubst elscreen-gf-db-directory (file-name &optional directory)
  (let ((directory (expand-file-name
                    (or directory
                        (and (buffer-file-name)
                             (file-name-directory (buffer-file-name)))
                        default-directory)))
        (previous-directory nil))
    (catch 'found
      (while (and directory
                  (not (and previous-directory
                            (string= directory previous-directory))))
        (if (file-exists-p (concat directory file-name))
            (throw 'found directory))
        (setq previous-directory directory)
        (setq directory (file-name-directory
                         (directory-file-name directory)))))))


;;; ElScreen-GF mode

(defconst elscreen-gf-mode-to-nickname-alist
  '(("^elscreen-gf-mode$" . (lambda ()
                              (format "ElScreen-GF%s"
                                      (if (and (elscreen-gf-process-exclusive-p
                                                elscreen-gf-idutils-mkid-process
                                                'noerror)
                                               (elscreen-gf-process-exclusive-p
                                                elscreen-gf-global-gtags-process
                                                'noerror))
                                          "" " (!)")))))
  "*Alist composed of the pair of mode-name and corresponding screen-name.")
(elscreen-set-mode-to-nickname-alist 'elscreen-gf-mode-to-nickname-alist)

(defun elscreen-gf-goto-screen-create (target-directory)
  (let ((buffer (get-buffer-create "ElScreen-GF")))
    (elscreen-find-and-goto-by-buffer buffer 'create)
    (switch-to-buffer buffer)
    (elscreen-gf-mode target-directory)
    (let ((buffer-read-only nil))
      (erase-buffer))))

(defvar elscreen-gf-pattern)
(defvar elscreen-gf-invoke-point)
(defvar elscreen-gf-filter-odd-string)
(defvar elscreen-gf-selected-entry-overlay)
(defun elscreen-gf-mode (target-directory)
  "Major mode for jumping to the entries.

Key bindings:
\\{elscreen-gf-mode-map}"
  (setq major-mode 'elscreen-gf-mode)
  (setq mode-name "ElScreen-GF")
  (use-local-map elscreen-gf-mode-map)
  (setq buffer-read-only t)
  (setq case-fold-search nil)
  (setq truncate-lines elscreen-gf-mode-truncate-lines)
  (set (make-local-variable 'elscreen-gf-pattern) nil)
  (set (make-local-variable 'elscreen-gf-invoke-point) nil)
  (set (make-local-variable 'elscreen-gf-filter-odd-string) nil)
  (set (make-local-variable 'elscreen-gf-selected-entry-overlay) nil)
  (setq default-directory target-directory)

  (auto-fill-mode nil))

(defun elscreen-gf-mode-selected-entry-overlay ()
  (elscreen-gf-move-overlay-create
   'elscreen-gf-selected-entry-overlay
   (point-at-bol) (point-at-eol)
   'elscreen-gf-mode-selected-entry-face))

(defun elscreen-gf-mode-next-line ()
  "Move the current entry vertically down."
  (interactive)
  (let ((current-line (line-number-at-pos)))
    (cond
     ((< current-line 4)
      (goto-line 4))
     ((< current-line (line-number-at-pos (point-max)))
      (forward-line 1)))
    (elscreen-gf-mode-selected-entry-overlay)))

(defun elscreen-gf-mode-previous-line ()
  "Move the current entry vertically up."
  (interactive)
  (let ((current-line (line-number-at-pos)))
    (cond
     ((< 4 current-line)
      (forward-line -1))
     (t
      (goto-line 4)))
    (elscreen-gf-mode-selected-entry-overlay)))

(defun elscreen-gf-mode-scroll-up ()
  "Scroll entries upward full screen."
  (interactive)
  (scroll-up)
  (elscreen-gf-mode-selected-entry-overlay))

(defun elscreen-gf-mode-scroll-down ()
  "Scroll entries downward full screen."
  (interactive)
  (scroll-down)
  (elscreen-gf-mode-selected-entry-overlay))

(defun elscreen-gf-mode-beginning-of-buffer ()
  "Move the current entry to the beginning of the entries."
  (interactive)
  (goto-line 4)
  (elscreen-gf-mode-selected-entry-overlay))

(defun elscreen-gf-mode-end-of-buffer ()
  "Move the current entry to the end of the entries."
  (interactive)
  (goto-char (point-max))
  (forward-line -1)
  (elscreen-gf-mode-selected-entry-overlay))

(defun elscreen-gf-mode-next-file ()
  (interactive)
  (cond
   ((save-excursion
      (beginning-of-line)
      (looking-at "^\\([^:\n]+\\):\\([0-9]+\\):"))
    (let ((file-name (match-string 1)))
      (goto-char (point-max))
      (re-search-backward (concat "^" file-name ":"))
      (forward-line)))
   (t
    (re-search-forward "^\\([^:\n]+\\):\\([0-9]+\\):" nil t)
    (beginning-of-line)))
  (elscreen-gf-mode-selected-entry-overlay))

(defun elscreen-gf-mode-previous-file ()
  (interactive)
  (cond
   ((save-excursion
      (beginning-of-line)
      (looking-at "^\\([^:\n]+\\):\\([0-9]+\\):"))
    (let ((file-name (match-string 1)))
      (goto-char (point-min))
      (re-search-forward (concat "^" file-name ":"))
      (forward-line -1)
      (beginning-of-line)))
   (t
    (re-search-backward "^\\([^:\n]+\\):\\([0-9]+\\):" nil t)))
  (when (looking-at "^\\([^:\n]+\\):\\([0-9]+\\):")
    (let ((file-name (match-string 1)))
      (goto-char (point-min))
      (re-search-forward (concat "^" file-name ":"))
      (beginning-of-line)))
  (elscreen-gf-mode-selected-entry-overlay))

(defun elscreen-gf-mode-truncate-lines-toggle ()
  "Toggle truncated lines."
  (interactive)
  (let ((window-start (window-start))
        (line-number-in-window 0))
    (beginning-of-line)
    (save-excursion
      (while (> (point) window-start)
        (setq line-number-in-window (1+ line-number-in-window))
        (vertical-motion -1)))
    (setq truncate-lines (not truncate-lines))
    (recenter line-number-in-window)))

(defvar elscreen-gf-emphasis-after-jump-overlay nil)
(defun elscreen-gf-mode-jump-to-entry ()
  "Jump to the current entry."
  (interactive)
  (when (save-excursion
          (beginning-of-line)
          (looking-at "^\\([^:\n]+\\):\\([0-9]+\\):"))
    (when (not (eq (marker-buffer (cdr elscreen-gf-invoke-point))
                   (current-buffer)))
      (ring-insert elscreen-gf-invoke-point-history elscreen-gf-invoke-point))
    (let ((file-name (match-string 1))
          (line (string-to-number (match-string 2)))
          (pattern elscreen-gf-pattern)
          token-chars nontoken-chars)
      (isearch-update-ring pattern)
      (elscreen-find-file file-name)
      (setq token-chars
            (or (elscreen-gf-major-mode-token-chars major-mode)
                (elscreen-gf-major-mode-token-chars 'elscreen-gf-mode)
                "a-zA-Z0-9"))
      (setq nontoken-chars (format "[^%s]" token-chars))
      (goto-line line)
      (let ((case-fold-search nil))
        (goto-char (or (and (re-search-forward
                             (format "\\(^\\|%s\\)\\(%s\\)\\(%s\\|$\\)"
                                     nontoken-chars pattern nontoken-chars)
                             (point-at-eol) t)
                            (match-beginning 2))
                       (and (search-forward pattern (point-at-eol) t)
                            (match-beginning 0))
                       (point-at-bol))))
      (elscreen-gf-move-overlay-create 'elscreen-gf-emphasis-after-jump-overlay
                                       (point-at-bol) (point-at-eol)
                                       'elscreen-gf-emphasis-after-jump-face))))

(defun elscreen-gf-mode-jump-to-entry-read-only ()
  (interactive)
  (elscreen-gf-mode-jump-to-entry)
  (toggle-read-only 'read-only))

(defun elscreen-gf-mode-search-quit (&optional force)
  (interactive)
  (when (and (not (elscreen-gf-process-exclusive-p
                   elscreen-gf-search-process 'noerror))
             (or force
                 (yes-or-no-p
                  (format "Really quit %s? "
                          (process-name elscreen-gf-search-process)))))
    (delete-process elscreen-gf-search-process)))

(defun elscreen-gf-mode-exit ()
  (interactive)
  (let* ((current-screen (elscreen-get-current-screen))
         (buffer (get-buffer "ElScreen-GF"))
         (screen (when (buffer-live-p buffer)
                   (elscreen-find-screen-by-buffer buffer))))
    (when screen
      (elscreen-goto-internal screen)
      (mapc
       (lambda (window)
         (if (one-window-p)
             (elscreen-kill screen)
           (delete-window window)))
       (get-buffer-window-list buffer)))
    (when buffer
      (kill-buffer buffer))
    (when (elscreen-screen-live-p screen)
      (elscreen-set-window-configuration
       screen (elscreen-current-window-configuration)))
    (when (elscreen-screen-live-p current-screen)
      (elscreen-goto-internal current-screen))))

;;; Fundamental functions shared among GNU grep/GNI ID Utils/cscope/GNU global

(defsubst elscreen-gf-process-exclusive-p (process &optional noerror)
  (let ((exclusive-p (not (and (processp process)
                               (eq (process-status process) 'run)))))
    (when (not (or exclusive-p noerror))
      (message "Sorry, %s is running now. Try again later."
               (process-name process)))
    exclusive-p))

(defsubst elscreen-gf-search-token-at-point ()
  (let* ((thing (or (get major-mode 'elscreen-gf-major-mode-thing) 'word))
         (thing-at-point (or (thing-at-point thing) "")))
    (set-text-properties 0 (length thing-at-point) nil thing-at-point)
    thing-at-point))

(defvar elscreen-gf-search-pattern-history nil)
(defsubst elscreen-gf-search-read-pattern (prompt &optional pattern-default)
  (let* ((pattern-default (or pattern-default
                              (elscreen-gf-search-token-at-point)
                              "")))
    (read-string prompt (cons pattern-default 0)
                 'elscreen-gf-search-pattern-history)))

(defun elscreen-gf-search-regexp-dot-to-token (regexp token)
  (let ((index 0)
        (token-length (length token)))
    (save-match-data
      (while (setq index (string-match "\\." regexp index))
        (if (and (/= index 0)
                 (string= (substring regexp (1- index) index) "\\"))
            (setq index (1+ index))
          (setq regexp (concat (substring regexp 0 index)
                               token
                               (substring regexp (1+ index))))
          (setq index (+ index token-length))))))
  regexp)

(defun elscreen-gf-read-selection (title prompt option-defs)
  (let ((candidate-buffer (generate-new-buffer " *ElScreen-GF-Read-Selection*"))
        candidate-window-height
        (minibuffer-map (copy-keymap minibuffer-local-map))
        window frame-last-window mini-hist)
    ;; prepare candidate buffer
    (with-current-buffer candidate-buffer
      (setq buffer-read-only t)
      (let ((buffer-read-only nil))
        (erase-buffer)
        (insert title "\n"
                (mapconcat
                 (lambda (option-def)
                   (apply 'format "  %s) %s\n" option-def))
                 option-defs nil))
        (goto-char (point-min))
        (save-excursion
          (while (not (eobp))
            (when (looking-at "^  \\([0-9a-z]\\)) .*$")
              (put-text-property
               (match-beginning 1) (match-end 1) 'face 'bold))
            (forward-line 1)))
        (setq candidate-window-height (line-number-at-pos (point-max)))
        (set-buffer-modified-p nil)))
    ;; prepare candidate window
    (save-window-excursion
      (setq frame-last-window
            (previous-window (static-if elscreen-on-xemacs
                                 (frame-highest-window)
                               (frame-first-window))))
      (while (minibuffer-window-active-p frame-last-window)
        (setq frame-last-window (previous-window frame-last-window)))
      (while (and (not (one-window-p))
                  (or (< (window-width frame-last-window)
                         (frame-width))
                      (< (window-height frame-last-window)
                         (+ candidate-window-height window-min-height))))
        (setq window frame-last-window)
        (setq frame-last-window (previous-window window))
        (delete-window window))
      (select-window (split-window frame-last-window))
      (shrink-window (- (window-height) candidate-window-height))
      (switch-to-buffer candidate-buffer)
      ;; make keymap for minibuffer
      (suppress-keymap minibuffer-map t)
      (define-key minibuffer-map "\C-m" 'undefined)
      (define-key minibuffer-map "\C-g" 'abort-recursive-edit)
      (mapcar
       (lambda (option-def)
         (define-key minibuffer-map (car option-def) 'self-insert-and-exit))
       option-defs)
      ;; read key from minibuffer
      (unwind-protect
          (assoc
           (read-from-minibuffer prompt nil minibuffer-map nil 'mini-hist)
           option-defs)
        (kill-buffer candidate-buffer)))))

(defvar elscreen-gf-search-process nil)
(defsubst elscreen-gf-run-search-command (command-name pattern command directory line-parser)
  (put 'elscreen-gf-mode 'elscreen-gf-major-mode-thing
       (or (get major-mode 'elscreen-gf-major-mode-thing) 'word))
  (let ((invoke-point (cons (elscreen-get-current-screen) (point-marker))))
    (elscreen-gf-goto-screen-create directory)
    (setq elscreen-gf-pattern pattern)
    (setq elscreen-gf-invoke-point invoke-point))
  (let ((buffer-read-only nil))
    (insert "DIR: " (abbreviate-file-name directory) "\n"
            "CMD: " command " (reformatted by gf)\n\n"))
  (message "Running %s..." command-name)
  (setq elscreen-gf-search-process
        (start-process command-name (current-buffer)
                       "sh" "-c" command "2> /dev/null"))
  (process-put elscreen-gf-search-process
               'elscreen-gf-search-line-parser line-parser)
  (set-process-filter elscreen-gf-search-process 'elscreen-gf-search-filter)
  (set-process-sentinel elscreen-gf-search-process 'elscreen-gf-search-sentinel))

(defun elscreen-gf-search-filter (process string)
  (set-buffer (process-buffer process))
  (save-excursion
    (let* ((string (concat elscreen-gf-filter-odd-string string))
           (token-chars (or (elscreen-gf-major-mode-token-chars major-mode)
                            "a-zA-Z0-9"))
           (nontoken-chars (format "[^%s]" token-chars))
           (line-parser (process-get process 'elscreen-gf-search-line-parser))
           (buffer-read-only nil))
      (goto-char (point-max))
      (save-excursion (insert string))
      (while (and (not (eobp)) (looking-at ".*\n"))
        (beginning-of-line)
        (let* ((line (buffer-substring (point-at-bol) (point-at-eol)))
               (line-elms (funcall line-parser line))
               (file-name (nth 0 line-elms))
               (line-number (nth 1 line-elms))
               (source (nth 2 line-elms))
               start end)
          (when line-elms
            (delete-region (point-at-bol) (point-at-eol))
            (elscreen-gf-overlay-create
             (point) (progn (insert (file-relative-name file-name)) (point))
             'elscreen-gf-mode-file-name-face)
            (insert ":")
            (elscreen-gf-overlay-create
             (point) (progn (insert line-number) (point))
             'elscreen-gf-mode-line-number-face)
            (insert ":")
            (save-excursion (insert (nth 0 source)))
            (forward-char (nth 1 source))
            (setq start (point) end (+ start (nth 2 source)))
            (while (or (and (eq (point) start)
                            (save-excursion
                              (re-search-forward
                               (format "\\(%s\\)\\(%s\\|$\\)"
                                       elscreen-gf-pattern nontoken-chars)
                               end t))
                            (eq (match-beginning 1) start))
                       (re-search-forward
                        (format "%s\\(%s\\)\\(%s\\|$\\)"
                                nontoken-chars elscreen-gf-pattern
                                nontoken-chars)
                        end t))
              (elscreen-gf-overlay-create (match-beginning 1) (match-end 1)
                                          'elscreen-gf-mode-pattern-face)
              (goto-char (match-end 1)))))
        (forward-line))
      (setq elscreen-gf-filter-odd-string
            (buffer-substring (point-at-bol) (point-at-eol)))
      (delete-region (point-at-bol) (point-at-eol)))))

(defun elscreen-gf-search-sentinel (process event)
  (let ((status (string-match "finished" event)))
    (message "Running %s... done" (process-name elscreen-gf-search-process))
    (setq elscreen-gf-search-process nil)
    (set-buffer (process-buffer process))
    (elscreen-gf-mode-selected-entry-overlay)))

;;; GNU grep

(defun elscreen-gf-grep (&optional pattern file-name-re)
  "Run grep, with user-specified args, and collect output
in the ElScreen-GF buffer."
  (interactive)
  (cond
   ((not (executable-find elscreen-gf-grep-program-name))
    (error "grep not found."))
   ((elscreen-gf-process-exclusive-p elscreen-gf-search-process)
    (let* ((pattern (or pattern
                        (elscreen-gf-search-read-pattern "Run grep (pattern): ")))
           (buffer-file-name (or (buffer-file-name) ""))
           (file-name-re-default (concat
                                  (file-name-directory buffer-file-name)
                                  "*"
                                  (when (file-name-extension buffer-file-name)
                                    ".")
                                  (file-name-extension buffer-file-name)))
           (file-name-re (or file-name-re
                             (read-string
                              "Run grep (files): "
                              (cons file-name-re-default 0) nil "*")))
           (token-chars (or (elscreen-gf-major-mode-token-chars major-mode)
                            "a-zA-Z0-9"))
           (token-chars-re (format "[%s]" token-chars))
           (nontoken-chars-re (format "[^%s]" token-chars))
           (pattern-tokenize (elscreen-gf-search-regexp-dot-to-token
                              pattern token-chars-re))
           (command (format "%s -IHn %s %s"
                            elscreen-gf-grep-program-name
                            (format "'\\(^\\|%s\\)\\(%s\\)\\(%s\\|$\\)'"
                                    nontoken-chars-re
                                    pattern-tokenize
                                    nontoken-chars-re)
                            file-name-re)))
      (elscreen-gf-run-search-command
       "grep" pattern-tokenize command default-directory
       'elscreen-gf-grep-line-parser)))))

(defun elscreen-gf-grep-line-parser (line)
  (when (string-match "^\\([^:\n]+\\):\\([0-9]+\\):\\(.*\\)$" line)
    (let ((file-name (match-string 1 line))
          (line-number (match-string 2 line))
          (source (match-string 3 line)))
      (list file-name line-number (list source 0 (length source))))))

;;; GNU ID Utils

(defsubst elscreen-gf-idutils-id-directory (&optional directory)
  (elscreen-gf-db-directory "ID" directory))

(defvar elscreen-gf-idutils-mkid-process nil)

(defun elscreen-gf-idutils-mkid (&optional directory)
  "Run mkid, with user-specified args."
  (interactive)
  (cond
   ((not (executable-find elscreen-gf-idutils-mkid-program-name))
    (error "mkid not found."))
   ((elscreen-gf-process-exclusive-p elscreen-gf-idutils-mkid-process)
    (let ((default-directory
            (or (and directory (file-directory-p directory) directory)
                (read-directory-name "Run mkid (target): "
                                     (or (elscreen-gf-idutils-id-directory)
                                         default-directory) nil t)))
          (command (format "%s" elscreen-gf-idutils-mkid-program-name)))
      (message "Running mkid...")
      (setq elscreen-gf-idutils-mkid-process
            (start-process "mkid" nil "sh" "-c" command))
      (set-process-sentinel elscreen-gf-idutils-mkid-process
                            'elscreen-gf-idutils-mkid-sentinel)
      (elscreen-notify-screen-modification 'force-immediately)))))

(defun elscreen-gf-idutils-mkid-sentinel (process event)
  (let ((status (string-match "finished" event)))
    (message "Running mkid... %s" (if status "done" "error"))
    (setq elscreen-gf-idutils-mkid-process nil)
    (elscreen-notify-screen-modification 'force-immediately)))

(defun elscreen-gf-idutils-mkid-after-save ()
  (when (and (buffer-file-name)
             (elscreen-gf-process-exclusive-p
              elscreen-gf-idutils-mkid-process 'noerror))
    (let ((id-directory (elscreen-gf-idutils-id-directory)))
      (when id-directory
        (elscreen-gf-idutils-mkid id-directory)))))

(defun elscreen-gf-idutils-mkid-setup-after-save-hook ()
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook 'elscreen-gf-idutils-mkid-after-save))

(defun elscreen-gf-idutils-gid (&optional pattern)
  "Run gid, with user-specified args, and collect output
in the ElScreen-GF buffer."
  (interactive)
  (cond
   ((not (executable-find elscreen-gf-idutils-gid-program-name))
    (when (yes-or-no-p "gid not found; execute grep instead? ")
      (elscreen-gf-grep)))
   ((not (elscreen-gf-idutils-id-directory))
    (funcall
     (nth 2 (elscreen-gf-read-selection
             "Cannot locate `ID'; what instead?"
             "Select action: "
             '(("m" "Execute mkid to generate `ID'" elscreen-gf-idutils-mkid)
               ("g" "Execute grep" elscreen-gf-grep))))))
   ((elscreen-gf-process-exclusive-p elscreen-gf-search-process)
    (let* ((pattern (or pattern
                        (elscreen-gf-search-read-pattern "Run gid (pattern): ")))
           (token-chars (or (elscreen-gf-major-mode-token-chars major-mode)
                            "a-zA-Z0-9"))
           (token-chars-re (format "[%s]" token-chars))
           (pattern-tokenize (elscreen-gf-search-regexp-dot-to-token
                              pattern token-chars-re))
           (directory (elscreen-gf-idutils-id-directory))
           (command (format "%s --regexp '^(%s)$'"
                            elscreen-gf-idutils-gid-program-name pattern)))
      ;; XXX: gid does not raise error even if given regexp starts with '*',
      ;; so we should check it here.
      (when (string-match "^\\*" pattern)
        (error "Invalid regexp: %s" pattern))
      (elscreen-gf-run-search-command
       "gid" pattern-tokenize command directory
       'elscreen-gf-grep-line-parser)))))

;;; cscope

(defsubst elscreen-gf-cscope-cross-ref-directory (&optional directory)
  (elscreen-gf-db-directory "cscope.out" directory))

(defun elscreen-gf-cscope (&optional pattern directory)
  "Run cscope, with user-specified args, and collect output
in the ElScreen-GF buffer."
  (interactive)
  (cond
   ((not (executable-find elscreen-gf-cscope-program-name))
    (when (yes-or-no-p "cscope not found; execute grep instead? ")
      (elscreen-gf-grep)))
   ((elscreen-gf-process-exclusive-p elscreen-gf-search-process)
    (let* ((pattern (or pattern
                        (elscreen-gf-search-read-pattern
                         "Run cscope (pattern): ")))
           (directory (or (and directory (file-directory-p directory) directory)
                          (elscreen-gf-cscope-cross-ref-directory)
                          (read-directory-name "Run cscope (target): "
                                               default-directory nil t)))
           (token-chars (or (elscreen-gf-major-mode-token-chars major-mode)
                            "a-zA-Z0-9"))
           (token-chars-re (format "[%s]" token-chars))
           (pattern-tokenize (elscreen-gf-search-regexp-dot-to-token
                              pattern token-chars-re))
           (query-type-def (elscreen-gf-read-selection
                            "Available query type for cscope: "
                            "Select query type: "
                            '(("0" "Find this C symbol" 0)
                              ("1" "Find this function definition" 1)
                              ("2" "Find functions calling this function" 3))))
           (query-type (nth 2 query-type-def))
           (command (format "%s -LR -%d '%s' | sort -k 1,1 -k 3,3n"
                            elscreen-gf-cscope-program-name query-type pattern))
           (line-parser (intern
                         (format "elscreen-gf-cscope-line-parser-query-type-%s"
                                 (car query-type-def)))))
      (elscreen-gf-run-search-command
       "cscope" pattern-tokenize command directory line-parser)))))

(defsubst elscreen-gf-cscope-line-parser-string-match (line)
  (string-match "^\\([^ ]+\\) \\([^ ]+\\) \\([0-9]+\\) \\(.*\\)$" line))

(defun elscreen-gf-cscope-line-parser-common (line)
  (when (elscreen-gf-cscope-line-parser-string-match line)
    (let ((file-name (match-string 1 line))
          (line-number (match-string 3 line))
          (function-name (match-string 2 line))
          (source (match-string 4 line)))
      (list file-name line-number
            (list (format "[%s] %s" function-name source)
                  (+ (length function-name) 3) (length source))))))

(defalias 'elscreen-gf-cscope-line-parser-query-type-0
  'elscreen-gf-cscope-line-parser-common)

(defun elscreen-gf-cscope-line-parser-query-type-1 (line)
  (when (elscreen-gf-cscope-line-parser-string-match line)
    (let ((file-name (match-string 1 line))
          (line-number (match-string 3 line))
          (source (match-string 4 line)))
      (list file-name line-number (list source 0 (length source))))))

(defalias 'elscreen-gf-cscope-line-parser-query-type-2
  'elscreen-gf-cscope-line-parser-common)

;;; GNU global

(defsubst elscreen-gf-global-tags-directory (&optional directory)
  (elscreen-gf-db-directory "GTAGS" directory))

(defvar elscreen-gf-global-gtags-process nil)

(defun elscreen-gf-global-gtags (&optional directory)
  "Run gtags, with user-specified args."
  (interactive)
  (cond
   ((not (executable-find elscreen-gf-global-gtags-program-name))
    (error "gtags not found."))
   ((elscreen-gf-process-exclusive-p elscreen-gf-global-gtags-process)
    (let ((default-directory
            (or (and directory (file-directory-p directory) directory)
                (read-directory-name "Run gtags (target): "
                                     (or (elscreen-gf-global-tags-directory)
                                         default-directory) nil t)))
          (command (format "%s" elscreen-gf-global-gtags-program-name)))
      (message "Running gtags...")
      (setq elscreen-gf-global-gtags-process
            (start-process "gtags" nil "sh" "-c" command))
      (set-process-sentinel elscreen-gf-global-gtags-process
                            'elscreen-gf-global-gtags-sentinel)
      (elscreen-notify-screen-modification 'force-immediately)))))

(defun elscreen-gf-global-gtags-sentinel (process event)
  (let ((status (string-match "finished" event)))
    (message "Running gtags... %s" (if status "done" "error"))
    (setq elscreen-gf-global-gtags-process nil)
    (elscreen-notify-screen-modification 'force-immediately)))

(defun elscreen-gf-global-gtags-after-save ()
  (when (and (buffer-file-name)
             (elscreen-gf-process-exclusive-p
              elscreen-gf-global-gtags-process 'noerror))
    (let ((tags-directory (elscreen-gf-global-tags-directory)))
      (when tags-directory
        (elscreen-gf-global-gtags tags-directory)))))

(defun elscreen-gf-global-gtags-setup-after-save-hook ()
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook 'elscreen-gf-global-gtags-after-save))

(defvar elscreen-gf-global-5.0-or-later-p)
(defsubst elscreen-gf-global-5.0-or-later-p ()
  (unless (boundp 'elscreen-gf-global-5.0-or-later-p)
    (let (global-version)
      (with-temp-buffer
        (call-process elscreen-gf-global-program-name nil t nil "--version")
        (setq global-version (string-to-number
                              (buffer-substring (point-min) (point-max)))))
      (setq elscreen-gf-global-5.0-or-later-p (and global-version
                                                   (<= 5.0 global-version)))))
  elscreen-gf-global-5.0-or-later-p)

(defun elscreen-gf-global (&optional pattern directory)
  "Run global, with user-specified args, and collect output
in the ElScreen-GF buffer."
  (interactive)
  (cond
   ((not (executable-find elscreen-gf-global-program-name))
    (when (yes-or-no-p "global not found; execute grep instead? ")
      (elscreen-gf-grep)))
   ((not (elscreen-gf-global-tags-directory))
    (funcall
     (nth 2 (elscreen-gf-read-selection
             "Cannot locate `GTAGS'; what instead?"
             "Select action: "
             '(("t" "Execute gtags to generate `GTAGS'" elscreen-gf-global-gtags)
               ("g" "Execute grep" elscreen-gf-grep))))))
   ((elscreen-gf-process-exclusive-p elscreen-gf-search-process)
    (let* ((pattern (or pattern
                        (elscreen-gf-search-read-pattern
                         "Run global (pattern): ")))
           (directory (or (and directory (file-directory-p directory) directory)
                          (elscreen-gf-global-tags-directory)
                          (read-directory-name "Run global (target): "
                                               default-directory nil t)))
           (token-chars (or (elscreen-gf-major-mode-token-chars major-mode)
                            "a-zA-Z0-9"))
           (token-chars-re (format "[%s]" token-chars))
           (pattern-tokenize (elscreen-gf-search-regexp-dot-to-token
                              pattern token-chars-re))
           (query-type (nth 2 (elscreen-gf-read-selection
                               "Available query type for GNU GLOBAL: "
                               "Select query type: "
                               '(("0" "Find this object definition" "")
                                 ("1" "Find this object references" " -r")))))
           (command (format
                     (cond
                      ((elscreen-gf-global-5.0-or-later-p)
                       "%s%s --result=grep -e '%s' | sort -t : -k 1,1 -k 2,2n")
                      (t
                       "%s -x%s -e '%s' | sort -k 3,3 -k 2,2n"))
                     elscreen-gf-global-program-name query-type pattern))
           (line-parser (if (elscreen-gf-global-5.0-or-later-p)
                            'elscreen-gf-grep-line-parser
                          'elscreen-gf-global-line-parser)))
      (elscreen-gf-run-search-command
       "global" pattern-tokenize command directory line-parser)))))

(defun elscreen-gf-global-line-parser (line)
  (when (string-match "^[^ ]+ +\\([0-9]+\\) +\\([^ ]+\\) \\(.*\\)$" line)
    (let ((file-name (match-string 2 line))
          (line-number (match-string 1 line))
          (source (match-string 3 line)))
    (list file-name line-number (list source 0 (length source))))))

;;; Going back to the latest invoke-point.

(defun elscreen-gf-go-back-to-latest-invoke-point ()
  (interactive)
  (while (not (or (ring-empty-p elscreen-gf-invoke-point-history)
                  (let ((invoke-point
                         (ring-ref elscreen-gf-invoke-point-history 0)))
                    (buffer-live-p (marker-buffer (cdr invoke-point))))))
    (ring-remove elscreen-gf-invoke-point-history 0))
  (if (ring-empty-p elscreen-gf-invoke-point-history)
      (message "No more invoke-points to go back to.")
    (let* ((invoke-point (ring-remove elscreen-gf-invoke-point-history 0))
           (invoke-point-screen (car invoke-point))
           (invoke-point-marker (cdr invoke-point))
           (invoke-point-buffer (marker-buffer invoke-point-marker))
           (invoke-point-position (marker-position invoke-point-marker))
           (invoke-point-buffer-screen-list
            (elscreen-find-screens
             `(lambda (screen)
                (elscreen-goto-internal screen)
                (not (null (get-buffer-window ,invoke-point-buffer))))))
           (get-marker-window
            (lambda (marker)
              (catch 'found
                (save-window-excursion
                  (mapc
                   (lambda (window)
                     (select-window window)
                     (when (eq (point) (marker-position marker))
                       (throw 'found window)))
                   (get-buffer-window-list (marker-buffer marker))))
                nil))))
      (cond
       ((null invoke-point-buffer-screen-list)
        ;; No screens showing invoke-point-buffer, so create new screen
        ;; and show it.
        (elscreen-create)
        (switch-to-buffer invoke-point-buffer)
        (goto-char invoke-point-position))
       ((memq invoke-point-screen invoke-point-buffer-screen-list)
        ;; invoke-point-screen shows invoke-point-buffer, so go to the
        ;; screen and select the window showing the buffer.  If there are
        ;; two or more window showing the buffer, we prefer the window
        ;; whose buffer's point is invoke-point-position.  Anyway,
        ;; we are moving point to it.
        (elscreen-goto invoke-point-screen)
        (select-window (or (funcall get-marker-window invoke-point-marker)
                           (get-buffer-window invoke-point-buffer)))
        (goto-char invoke-point-position))
       ((catch 'found
          (mapc
           (lambda (screen)
             (let (window)
               (when (save-window-excursion
                       (elscreen-goto-internal screen)
                       (setq window (funcall get-marker-window
                                             invoke-point-marker)))
                 ;; There is (at least one) screen showing buffer whose
                 ;; point is invoke-point-position, so show it.
                 (elscreen-goto screen)
                 (select-window window)
                 (throw 'found t))))
           invoke-point-buffer-screen-list)))
       (t
        ;; invoke-point-screen does not show invoke-point-buffer, and
        ;; the others does not show invoke-point-buffer whose point is
        ;; invoke-point-position; so we're showing one of screens showing
        ;; invoke-point-buffer and moving point to invoke-point-position.
        (elscreen-goto (car invoke-point-buffer-screen-list))
        (select-window (get-buffer-window invoke-point-buffer))
        (goto-char (invoke-point-position)))))))

;;; Help

(defvar elscreen-gf-help "ElScreen-GF keys:
  \\[elscreen-gf-grep]    Run grep
  \\[elscreen-gf-idutils-mkid]    Run mkid (GNU ID Utils)
  \\[elscreen-gf-idutils-gid]    Run gid (GNU ID Utils)
  \\[elscreen-gf-cscope]    Run cscope
  \\[elscreen-gf-global-gtags]    Run gtags (GNU GLOBAL)
  \\[elscreen-gf-global]    Run global (GNU GLOBAL)
  \\[elscreen-gf-go-back-to-latest-invoke-point]    Go back to the latest invoke-point.
  \\[elscreen-gf-display-version]    Display ElScreen-GF version")
(elscreen-set-help 'elscreen-gf-help)

(defun elscreen-gf-display-version ()
  "Display ElScreen-GF version."
  (interactive)
  (elscreen-message (concat "ElScreen-GF version " elscreen-gf-version)))
