;;; rect+.el --- Extensions for rect.el

;; Author: Hayashi Masahiro <mhayashi1120@gmail.com>
;; Keywords: rectangle edit
;; URL: TODO

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

;; rect+.el provides extensions for rect.el

;;; Install:

;; Put this file into load-path'ed directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.
;;
;;     (require 'rect+)
;;     (define-key ctl-x-r-map "C" 'rectplus-copy-rectangle)
;;     (define-key ctl-x-r-map "N" 'rectplus-insert-number-rectangle)
;;     (define-key ctl-x-r-map "\M-c" 'rectplus-create-rectangle-by-regexp)
;;     (define-key ctl-x-r-map "A" 'rectplus-append-rectangle-to-eol)
;;     (define-key ctl-x-r-map "R" 'rectplus-kill-ring-to-rectangle)
;;     (define-key ctl-x-r-map "K" 'rectplus-rectangle-to-kill-ring)
;;     (define-key ctl-x-r-map "\M-l" 'rectplus-downcase-rectangle)
;;     (define-key ctl-x-r-map "\M-u" 'rectplus-upcase-rectangle)


;;; Code:

(require 'rect)
(require 'replace)

(defvar current-prefix-arg)

(defun rectplus-rectangle-to-kill-ring ()
  "Killed rectangle to normal `kill-ring'.
After executing this command, you can type \\[yank]."
  (interactive)
  (with-temp-buffer
    (yank-rectangle)
    ;;avoid message
    (let (message-log-max)      
      (message ""))
    (kill-new (buffer-string)))
  (message (substitute-command-keys
	    (concat "Killed rectangle converted to normal text. "
		    "You can type \\[yank] now."))))

(defun rectplus-kill-ring-to-rectangle (&optional succeeding)
  "Make rectangle from clipboard or `kill-ring'. 
After executing this command, you can type \\[yank-rectangle]."
  (interactive 
   (let (str)
     (when current-prefix-arg 
       (setq str (read-from-minibuffer "Succeeding string to killed: ")))
     (list str)))
  (let ((tab tab-width))
    (with-temp-buffer
      ;; restore
      (setq tab-width tab)
      (insert (current-kill 0))
      (goto-char (point-min))
      (let ((max 0)
	    str len list)
	(while (not (eobp))
	  (setq str (buffer-substring (line-beginning-position) (line-end-position)))
	  (when succeeding
	    (setq str (concat str succeeding)))
	  (setq len (length str))
	  (when (> len max)
	    (setq max len))
	  (setq list (cons str list))
	  (forward-line 1))
	(setq killed-rectangle 
	      (rectplus-non-rectangle-to-rectangle (nreverse list) max)))))
  (message (substitute-command-keys 
	    (concat "Killed text converted to rectangle. "
		    "You can type \\[yank-rectangle] now."))))

(defun rectplus-append-rectangle-to-eol (&optional preceeding)
  "Append killed rectangle to end-of-line sequentially."
  (interactive
   (let (str)
     (when current-prefix-arg 
       (setq str (read-from-minibuffer "Preceeding string to append: ")))
     (list str)))
  (unless preceeding
    (setq preceeding ""))
  (save-excursion
    (mapc
     (lambda (x)
       (goto-char (line-end-position))
       (insert preceeding)
       (insert x)
       (forward-line 1))
     killed-rectangle)))

(defun rectplus-copy-rectangle (start end)
  "Copy rectangle area."
  (interactive "r")
  (deactivate-mark)
  (setq killed-rectangle (extract-rectangle start end)))

(defun rectplus-insert-number-rectangle (start end number-string &optional step)
  "Insert incremental number into each left edges of rectangle's line

Only effect to region if region is activated.

NUMBER-STRING indicate start number and inserted format.
  \"1\"   => [\"1\" \"2\" \"3\" ...]
  \"001\" => [\"001\" \"002\" \"003\" ...]
  \" 1\"  => [\" 1\" \" 2\" \" 3\" ...]

STEP is incremental count. Default is 1.
"
  (interactive 
   (progn
     (unless mark-active
       (signal 'mark-inactive nil))
     (let ((beg (region-beginning))
	   (fin (region-end))
	   number step)
       (setq number (rectplus-read-from-minibuffer "Number: " "^[ ]*[0-9]*$"))
       (when current-prefix-arg
	 (setq step (rectplus-read-number "Step: " 1)))
       (deactivate-mark)
       (list beg fin number step))))
  (setq step (or step 1))
  (save-excursion 
    (let ((fmtlen (number-to-string (length number-string)))
	  (end-marker (set-mark end))
	  (l 0)
	  (lines 0)
	  rect-lst padchar num)
      (save-excursion
	(goto-char start)
	(while (and (<= (point) end)
		    (not (eobp)))
	  (forward-line 1)
	  (setq lines (1+ lines))))
      (when (string-match "^\\([0 ]\\)" number-string)
	(setq padchar (match-string 1 number-string)))
      (setq num (string-to-number number-string))
      (delete-rectangle start end)
      (while (< l lines)
	(setq rect-lst (cons (format (concat "%" padchar fmtlen "d") num) rect-lst))
	(setq num (+ step num)
	      l (1+ l)))
      (goto-char start)
      (insert-rectangle (nreverse rect-lst)))))

(defun rectplus-create-rectangle-by-regexp (start end regexp)
  "Capture string matching to REGEXP.
Only effect to region if region is activated.
"
  (interactive 
   (let* ((beg (if mark-active (region-beginning) (point-min)))
	  (end (if mark-active (region-end) (point-max)))
	  (regexp (rectplus-read-regexp "Regexp")))
     (list beg end regexp)))
  (let ((max 0)
	str len list)
    (save-excursion
      (save-restriction 
	(narrow-to-region start end)
	(goto-char (point-min))
	(while (re-search-forward regexp nil t)
	  (setq str (match-string 0))
	  (setq len (string-width str))
	  (setq list (cons str list))
	  (when (> len max)
	    (setq max len)))))
    ;; fill by space
    (setq killed-rectangle 
	  (rectplus-non-rectangle-to-rectangle (nreverse list) max))))

(defun rectplus-upcase-rectangle (start end)
  (interactive "*r")
  (rectplus-do-translate start end 'upcase))

(defun rectplus-downcase-rectangle (start end)
  (interactive "*r")
  (rectplus-do-translate start end 'downcase))

(defun rectplus-do-translate (start end translator)
  "TRANSLATOR is function accept one string argument and return string."
  (apply-on-rectangle 
   (lambda (s e)
     (let* ((start (progn (move-to-column s) (point)))
	    (end (progn (move-to-column e) (point)))
	    (current (buffer-substring start end))
	    (new (funcall translator current)))
       (unless (string= current new)
	 (delete-region start end)
	 (insert new))))
   start end))

(defun rectplus-read-from-minibuffer (prompt must-match-regexp &optional default)
  "Check input string by MUST-MACH-REGEXP.
See `read-from-minibuffer'."
  (let (str)
    (while (null str)
      (setq str (read-from-minibuffer prompt default))
      (unless (string-match must-match-regexp str)
	(message "Invalid string!")
	(sit-for 0.5)
	(setq str nil)))
    str))

(defun rectplus-read-number (prompt default)
  (string-to-number (rectplus-read-from-minibuffer 
		     prompt "^[0-9]+$" 
		     (number-to-string default))))

(defun rectplus-non-rectangle-to-rectangle (strings &optional max)
  (let ((fmt (concat "%-" (number-to-string max) "s")))
    (mapcar
     (lambda (s)
       (format fmt s))
     strings)))

(defun rectplus-read-regexp (prompt)
  (if (fboundp 'read-regexp)
      (read-regexp prompt)
    (read-from-minibuffer (concat prompt ": "))))

(provide 'rect+)

;;; rect+.el ends here
