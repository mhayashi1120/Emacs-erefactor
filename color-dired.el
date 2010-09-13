;;; color-dired.el --- Make dired color.

;; Author: Hayashi Masahiro <mhayashi1120@gmail.com>
;; Keywords: dired color
;; URL: http://github.com/mhayashi1120/Emacs-Lisp/raw/master/color-dired.el
;; Emacs: GNU Emacs 22 or later

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; color-dired provides colored text that recently changed. (`Today', `This week'
;; ,`Last week' and `Last week before')

;;; Install:

;; Put this file into load-path'ed directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.
;;
;;     (require 'color-dired)

;;; History:

;; This program is inspired from following url.
;; http://www.bookshelf.jp/soft/meadow_25.html#SEC288
;; http://homepage1.nifty.com/blankspace/emacs/dired.html

;;; Usage:

;; You can change the face by changing following variables.
;; `color-dired-changed-today-face'
;; `color-dired-changed-this-week-face'
;; `color-dired-changed-last-week-face'
;; `color-dired-changed-last-week-before-face'

;;; TODO:
;; * (require 'ls-lisp) destroy this feature.

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar emacs-major-version)

(defgroup color-dired nil
  "Colored dired."
  :group 'dired
  :prefix "color-dired-")

(defface color-dired-changed-today-default-face 
  '(
    (((class color)
      (background light))
     (:foreground "Green" :weight bold))
    (((class color)
      (background dark))
     (:foreground "light green" :weight bold)))
  "Font lock mode face used to highlight changed in this day."
  :group 'color-dired)

(defface color-dired-changed-this-week-default-face 
  '(
    (((class color)
      (background light))
     (:foreground "SpringGreen" :weight bold))
    (((class color)
      (background dark))
     (:foreground "SpringGreen" :weight bold)))
  "Font lock mode face used to highlight changed in this week."
  :group 'color-dired)

(defface color-dired-changed-last-week-default-face 
  '(
    (((class color)
      (background light))
     (:foreground "MediumSpringGreen"))
    (((class color)
      (background dark))
     (:foreground "MediumSpringGreen")))
  "Font lock mode face used to highlight changed in last week."
  :group 'color-dired)

(defface color-dired-changed-last-week-before-default-face
  '(
    (((class color)
      (background light))
     (:foreground "GreenYellow"))
    (((class color)
      (background dark))
     (:foreground "GreenYellow")))
  "Font lock mode face used to highlight changed in last week before."
  :group 'color-dired)

(defcustom color-dired-changed-today-face 
  'color-dired-changed-today-default-face
  "Font lock mode face used to highlight changed in this day."
  :type 'face
  :group 'color-dired)

(defcustom color-dired-changed-this-week-face 
  'color-dired-changed-this-week-default-face
  "Font lock mode face used to highlight changed in this week."
  :type 'face
  :group 'color-dired)

(defcustom color-dired-changed-last-week-face 
  'color-dired-changed-last-week-default-face
  "Font lock mode face used to highlight changed in last week."
  :type 'face
  :group 'color-dired)

(defcustom color-dired-changed-last-week-before-face 
  'color-dired-changed-last-week-before-default-face
  "Font lock mode face used to highlight changed in last week before."
  :type 'face
  :group 'color-dired)

(defvar color-dired-last-week-before-regexp nil)
(defvar color-dired-last-week-regexp nil)
(defvar color-dired-this-week-regexp nil)

(defun color-dired-guessed-date-format ()
  (let (template date)
    (with-temp-buffer
      (insert-directory "~" "-la")
      (setq template (member-if 
		      (lambda (x) (string-match "^d[[:ascii:]]+$" x))
		      (split-string (buffer-string)))))
    (setq date (nth 5 template))
    (cond
     ((null date)
      "%b %e")
     ((string-match "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" date)
      "%Y-%m-%d")
     ((string-match "^[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" date)
      "%y-%m-%d")
     (t
      "%b %e"))))

;; `regexp-opt' is generating different regexp.
(if (> emacs-major-version 22)
    (defun color-dired-regexp-opt (strings)
      (regexp-opt strings))
  (defun color-dired-regexp-opt (strings)
    (concat "\\(?:" (regexp-opt strings) "\\)")))

(defcustom color-dired-date-format (color-dired-guessed-date-format)
  "*Format of dired displaying. See `format-time-string'" 
  :group 'color-dired
  :type 'string)

(defcustom color-dired-time-regexp 
  (concat
   " "
   (color-dired-regexp-opt
    (append
     (mapcar (lambda (x) (format "%02d" x)) (number-sequence 0 23))
     (mapcar (lambda (x) (format "%2d" x)) (number-sequence 0 23))
     (mapcar 'number-to-string (number-sequence 0 23))))
   ":[0-5][0-9]")
  "*Time format append to `color-dired-date-format'"
  :group 'color-dired
  :type 'string)

(defconst color-dired-search-keywords
  '((color-dired-today-search . color-dired-changed-today-face)
    (color-dired-this-week-search . color-dired-changed-this-week-face)
    (color-dired-last-week-search . color-dired-changed-last-week-face)
    (color-dired-last-week-before-search . color-dired-changed-last-week-before-face)))

(defmacro color-dired-set-week-regexp (var num start-with)
  `(when (or (null ,var)
	     (null (get ',var 'day))
	     ;; compare to current time
	     (> (string-to-number (format-time-string "%Y%m%d"))
		(get ',var 'day)))
     (set ',var
	  (color-dired-generate-regexp ,num ,start-with))
     (put ',var 'day (string-to-number (format-time-string "%Y%m%d")))))

(defun color-dired-time-sequence (num start-seconds)
  (let ((step (* 24 60 60))
	(i 0)
	time result)
    (setq time start-seconds)
    (while (< i num)
      (setq result 
	    (cons (seconds-to-time time) result))
      (setq time (+ step time))
      (setq i (1+ i)))
    (nreverse result)))

(defun color-dired-diff-seconds (days)
  (* 
   ;; sec per day
   24 60 60.0 
   ;; past day from last week sunday (except today)
   (+ (color-dired-week-number) days)))

(defun color-dired-week-number ()
  (string-to-number (format-time-string "%w")))

(defun color-dired-generate-regexp (num start-seconds)
  "return NUM of day regexp depend upon `color-dired-date-format'.
START-SECONDS means start time as float value.
"
  (when color-dired-date-format
    (let* ((times (color-dired-time-sequence num start-seconds))
	   (day-list (mapcar
		      (lambda (time)
			(format-time-string color-dired-date-format time))
		      times)))
      (and 
       day-list 
       (concat " " 
	       (color-dired-regexp-opt day-list) 
	       color-dired-time-regexp
	       " ")))))

(defun color-dired-today-search (bound)
  "font-lock search function for dired."
  (re-search-forward
   (color-dired-generate-regexp 1 (float-time)) bound t))

(defun color-dired-this-week-search (bound)
  (color-dired-set-week-regexp 
   color-dired-this-week-regexp 
   (color-dired-week-number)
   (- (float-time) 
      (color-dired-diff-seconds 0)))
  (and color-dired-this-week-regexp
       (re-search-forward color-dired-this-week-regexp bound t)))

(defun color-dired-last-week-search (bound)
  (color-dired-set-week-regexp 
   color-dired-last-week-regexp 
   7
   (- (float-time) 
      (color-dired-diff-seconds 7)))
  (and color-dired-last-week-regexp
       (re-search-forward color-dired-last-week-regexp bound t)))

(defun color-dired-last-week-before-search (bound)
  (color-dired-set-week-regexp 
   color-dired-last-week-before-regexp 
   7
   (- (float-time) 
      (color-dired-diff-seconds 14)))
  (and color-dired-last-week-before-regexp
       (re-search-forward color-dired-last-week-before-regexp bound t)))

(font-lock-add-keywords 'dired-mode color-dired-search-keywords)

(provide 'color-dired)

;;; color-dired.el ends here
