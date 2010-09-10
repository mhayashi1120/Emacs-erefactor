;;; find-select.el --- find file utilities

;; Author: Hayashi Masahiro <mhayashi1120@gmail.com>
;; Keywords: grep edit result writable
;; URL: http://gist.github.com/556053.txt

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

;; You can use `find' command-line option like S Expression.
;; Make easy way of editing find complex arguments and to display
;; full command-line to small buffer.
;;

;;; Install:

;; Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put the following expression into your ~/.emacs.
;;
;;     (require 'find-select)

;; ** In Emacs 22 or earlier **
;; Not tested. But to install find-cmd.el from following url may work.
;; http://repo.or.cz/w/emacs.git/blob_plain/HEAD:/lisp/find-cmd.el

;;; Usage:

;; Following command open editable buffer.
;;
;;    M-x find-select
;; 
;; You can edit `find' command-line option like s-expression following.
;;
;; (or (name "HOGE") (type "f")) (type "d")
;;
;; This expand to 
;;
;; find /some/directory \( -name HOGE -or -type f \) -type d 
;;
;; Type C-c C-c execute above command and display command output.
;; Type C-c C-q quit editing.
;; Type M-n, M-p move history when exists.

;;; History:
;; 

;;; TODO
;; * Display find process status.
;; * Can call function.
;; * Describe how to call command.
;; * Can complete symbol.

;;; Code:

(require 'find-cmd)

(defvar find-program)

(defvar find-select-buffer-name " *Find Select* ")
(defvar find-select-result-buffer-name " *Find Select Results* ")
(defvar find-select-sub-buffer-name " *Find Select Command-Line* ")
(defvar find-select-previous-window-configuration nil)
(defvar find-select-history nil)
(defvar find-select-history-position nil)

(defvar find-select-mode-map nil)

(unless find-select-mode-map
  (setq find-select-mode-map
	(let ((map (make-sparse-keymap)))

	  (define-key map "\C-c\C-k" 'find-select-quit)
	  (define-key map "\C-c\C-q" 'find-select-quit)
	  (define-key map "\C-c\C-c" 'find-select-execute)
	  (define-key map "\M-p" 'find-select-previous-history)
	  (define-key map "\M-n" 'find-select-next-history)

	  map)))
    
(defvar find-select-result-mode-map nil)

(unless find-select-result-mode-map
  (setq find-select-result-mode-map
	(let ((map (make-sparse-keymap)))

	  (define-key map "\C-c\C-c" 'find-select-kill-process)
	  (define-key map "\C-c\C-k" 'find-select-quit)
	  (define-key map "\C-c\C-q" 'find-select-quit)
	  (define-key map "\C-c!" 'find-select-start-with-xargs)
	  (define-key map "\C-c\e|" 'find-select-shell-command)

	  map)))

(define-minor-mode find-select-minor-mode
  "Minor mode to build `find' command args by using `fsvn-cmd'"
  nil nil find-select-mode-map
  (set (make-local-variable 'after-change-functions) nil)
  (set (make-local-variable 'kill-buffer-hook) nil)
  (set (make-local-variable 'find-select-previous-window-configuration) nil)
  (set (make-local-variable 'find-select-history-position) nil)
  (set (make-local-variable 'window-configuration-change-hook) nil)
  (add-hook 'after-change-functions 'find-select-show-command)
  (add-hook 'kill-buffer-hook 'find-select-cleanup)
  (message (substitute-command-keys 
	    (concat "Type \\[find-select-execute] to execute find, "
		    "\\[find-select-quit] to quit edit."))))

(defun find-select-result-mode ()
  (kill-all-local-variables)
  (use-local-map find-select-result-mode-map)
  (setq major-mode 'find-select-result-mode)
  (setq mode-name "Find Select Results")
  (erase-buffer)
  (set-buffer-modified-p nil)
  (setq buffer-undo-list nil))

(defun find-select-cleanup ()
  (let ((buf (get-buffer find-select-sub-buffer-name)))
    (when (buffer-live-p buf)
      (kill-buffer buf)))
  (when find-select-previous-window-configuration
    (set-window-configuration find-select-previous-window-configuration)))

(defun find-select-read-function ()
  (let (collection tmp)
    (mapatoms
     (lambda (s)
       (let (f args)
	 (when (fboundp s)
	   (setq f (symbol-function s))
	   (setq args (help-function-arglist f))
	   (when (and (listp args) (> (length args) 0))
	     (setq collection (cons f collection))))))
     obarray)
    (setq tmp (completing-read "Function (one arg): " collection nil t nil nil))
    (if tmp
	(intern tmp)
      'identity)))

(defun find-select-find-filter (process event)
  (with-current-buffer (process-buffer process)
    (save-excursion 
      (goto-char (point-max))
      (insert event)
      (set-buffer-modified-p nil))))

(defun find-select-show-command (&rest dummy)
  (condition-case nil
      (let ((buf (get-buffer-create find-select-sub-buffer-name))
	    args win parse-error)
	(condition-case err
	    (setq args (find-select-args-string))
	  (error (setq parse-error err)))
	(with-current-buffer buf
	  (let ((inhibit-read-only t)
		buffer-read-only)
	    (erase-buffer)
	    (cond
	     (args
	      (insert (find-select-put-face (concat find-program " " default-directory " ")
					    'font-lock-constant-face))
	      (insert (find-select-put-face args 'font-lock-variable-name-face) "\n"))
	     (t
	      (insert (find-select-put-face (format "%s" parse-error)
					    'font-lock-warning-face)))))
	  (setq buffer-read-only t)
	  (set-buffer-modified-p nil))
	(unless (memq buf (mapcar 'window-buffer (window-list)))
	  (setq win (split-window-vertically))
	  (set-window-buffer win buf)
	  (set-window-text-height win 5)))
    ;; ignore all
    (error nil)))

(defun find-select-put-face (string face)
  (put-text-property 0 (length string) 'face face string)
  string)

(defun find-select-args-string ()
  (let ((subfinds (find-select-read-expressions)))
    (mapconcat 'find-to-string subfinds "")))

(defun find-select-args-string-safe ()
  (condition-case nil
      (find-select-args-string)
    (error nil)))

(defun find-select-args ()
  ;;TODO escaped space
  (split-string (find-select-args-string-safe) " " t))

(defun find-select-read-expressions ()
  (let (exp subfinds)
    (save-excursion
      (goto-char (point-min))
      (condition-case nil
	  (while (setq exp (read (current-buffer)))
	    (setq subfinds (cons exp subfinds)))
	(end-of-file nil)))
    (nreverse subfinds)))

(defun find-select-goto-history (previous)
  (let ((n (funcall (if previous '1+ '1-) (or find-select-history-position -1))))
    (cond
     ((or (null find-select-history)
	  (< n 0))
      (message "No more history"))
     ((> n (1- (length find-select-history)))
      (message "No more history"))
     (t
      (erase-buffer)
      (insert (nth n find-select-history))
      (setq find-select-history-position n)))))

(defun find-select-create-temp ()
  (let ((temp (make-temp-file "EmacsFind"))
	(coding-system-for-write file-name-coding-system))
    (write-region (point-min) (point-max) temp)
    temp))

(defun find-select-concat-0 ()
  (let (list)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(setq list (cons 
		    (buffer-substring (line-beginning-position)
				      (line-end-position))
		    list))
	(forward-line 1)))
    (setq list (nreverse list))
    (mapconcat 'identity list "\000")))



(defun find-select-previous-history ()
  (interactive)
  (find-select-goto-history t))

(defun find-select-next-history ()
  (interactive)
  (find-select-goto-history nil))

(defun find-select-quit ()
  "Quit editing."
  (interactive)
  (mapc
   (lambda (name)
     (let ((buffer (get-buffer name)))
       (when (buffer-live-p buffer)
	 (kill-buffer buffer))))
   (list find-select-result-buffer-name find-select-buffer-name)))

(defun find-select-kill-process ()
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (unless (and proc (eq (process-status proc) 'run))
      (error "No process to kill"))
    (when (y-or-n-p "Find process is running. Kill it? ")
      (delete-process proc))))

(defun find-select-execute (&optional arg)
  "Execute `find' with editing args.
Optional ARG means execute `find-dired' with same arguments."
  (interactive "P")
  (let ((main-buffer (current-buffer))
	find-args)
    (if arg
	(progn
	  ;;TODO use find-select-args-string ?
	  ;;TODO when error?
	  (setq find-args (find-select-args-string-safe))
	  (find-select-commit main-buffer)
	  (find-dired default-directory find-args))
      (let ((buffer (get-buffer-create find-select-result-buffer-name))
	    (dir default-directory)
	    proc)
	(setq find-args (find-select-args))
	(with-current-buffer buffer
	  (let (command-line)
	    (setq command-line (format "%s %s" find-program 
				       (mapconcat 'identity find-args " ")))
	    (find-select-result-mode)
	    (setq default-directory dir)
	    ;;TODO for cmd.exe
	    (setq proc (start-process "Select file by find" (current-buffer) 
				      shell-file-name "-c" command-line))
	    (set-process-sentinel proc (lambda (p e)))
	    (set-process-filter proc 'find-select-find-filter)))
	(find-select-commit main-buffer)
	(switch-to-buffer buffer)))))

;; TODO
(defun find-select-start-with-xargs ()
  (interactive)
  (error "Not implement yet")
  ;; (let ((infile (find-select-create-temp)))
  ;;   (call-process "xargs" infile ))
  )

;;TODO
(defun find-select-shell-command ()
  (interactive)
  (error "Not implement yet"))

(defun find-select-commit (buffer)
  (add-to-history 'find-select-history (with-current-buffer buffer (buffer-string)))
  (kill-buffer buffer))

;;TODO
(defun find-select-call-function ()
  (interactive)
  )

(defun find-select ()
  ;; execute find and display command-line to buffer.
  ;; electric mode
  ;; execute buffer buffer with call-process-region
  (interactive)
  (let ((buffer (get-buffer-create find-select-buffer-name))
	(dir default-directory)
	(config (current-window-configuration)))
    (with-current-buffer buffer
      (setq default-directory dir)
      (emacs-lisp-mode)
      (find-select-minor-mode 1)
      (setq find-select-previous-window-configuration config)
      (erase-buffer)
      (set-buffer-modified-p nil))
    (select-window (display-buffer buffer))))



(provide 'find-select)

;;; find-select.el ends here
