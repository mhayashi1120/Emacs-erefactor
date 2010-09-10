;;; pcsv.el --- Parser of csv

;; Author: Hayashi Masahiro <mhayashi1120@gmail.com>
;; Keywords: csv parse rfc4180
;; URL: http://gist.github.com/573561.txt
;; Emacs: GNU Emacs 21 or later

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

;; pcsv provides parser of csv based on rfc4180
;; http://www.rfc-editor.org/rfc/rfc4180.txt

;;; Install:

;; Put this file into load-path'ed directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.
;;
;;     (require 'pcsv)

;;; Code:

(defun pcsv-parse-buffer (&optional buffer)
  "Parse a current buffer as a csv.
BUFFER non-nil means parse buffer instead of current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (save-restriction
      (widen)
      (pcsv-parse-region (point-min) (point-max)))))

(defun pcsv-parse-file (file &optional coding-system)
  "Parse FILE as a csv file."
  (with-temp-buffer
    (let ((coding-system-for-read coding-system))
      (insert-file-contents file))
    (pcsv-parse-region (point-min) (point-max))))

(defvar pcsv-eobp)

(defun pcsv-parse-region (start end)
  "Parse region as a csv."
  (let (pcsv-eobp)
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(goto-char (point-min))
	(let (v line ret)
	  (while (setq v (pcsv-read))
	    (setq line (cons v line))
	    (when (bolp)
	      (setq ret (cons (nreverse line) ret))
	      (setq line nil)))
	  (when line
	    (setq ret (cons (nreverse line) ret)))
	  (nreverse ret))))))

(defun pcsv-read ()
  (cond 
   ((and (not pcsv-eobp)
	 (eobp)
	 (char-before)
	 (= (char-before) ?,))
    (setq pcsv-eobp t)
    "")
   ((eobp)
    nil)
   ((looking-at "\"")
    (unless (looking-at "\"\\(\\(?:\"\"\\|[^\"]\\)*\\)\"\\(?:,\\|\n\\|$\\)")
      (signal 'invalid-read-syntax nil))
    (prog1 
	(pcsv-unquote-string (match-string 1))
      (goto-char (match-end 0))))
   ((looking-at "\\([^\n,]*\\)\\(?:,\\|\n\\|$\\)")
    (prog1
	(match-string 1)
      (goto-char (match-end 0))))
   (t
    ;; never through here.
    (signal 'invalid-read-syntax nil))))

(defun pcsv-unquote-string (string)
  (let ((list (string-to-list string))
	ret)
    (while list
      (cond
       ((and (eq (car list) ?\")
	     (eq (cadr list) ?\"))
	(setq ret (cons (car list) ret))
	(setq list (cdr list)))
       (t
	(setq ret (cons (car list) ret))))
      (setq list (cdr list)))
    (concat (nreverse ret))))



;;
;; unit test
;;

(when (require 'el-expectations nil t)
  (defun pcsv-test-get (csv-string)
    (with-temp-buffer
      (insert csv-string)
      (pcsv-parse-buffer)))

  (dont-compile
    (expectations 
      (expect '()
	(pcsv-test-get ""))
      (expect '((""))
	(pcsv-test-get "\n"))
      (expect '(("a"))
	(pcsv-test-get "a\n"))
      (expect '(("a"))
	(pcsv-test-get "a"))
      (expect '(("a" ""))
      	(pcsv-test-get "a,"))
      (expect '(("a" "b" "c") ("a,a" "bb\n" "c,\nc") ("\"aaa\"" ",\","))
	(pcsv-test-get "a,b,c\n\"a,a\",\"bb\n\",\"c,\nc\"\n\"\"\"aaa\"\"\",\",\"\",\"\n"))
      (expect (error invalid-read-syntax)
	(pcsv-test-get "\"a"))
      )))
;; (expectations-execute)

(provide 'pcsv)

;;; pcsv.el ends here
