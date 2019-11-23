;;; erefactor-test.el --- Erefactor tests

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; URL: https://github.com/mhayashi1120/Emacs-erefactor

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

;; Simple refactoring, linting utilities for Emacs-Lisp.

;;; TODO:

;; * more test

;;; Code:

(require 'ert)

;;
;; unit test
;;

(ert-deftest erefactor-local-bindings ()
  :tags '(erefactor)

  (should (erefactor--local-binding-p 'v '(defun f (v))))
  (should (erefactor--local-binding-p 'v '(lambda (v))))
  (should (erefactor--local-binding-p 'v '(let ((v v1)))))
  (should (erefactor--local-binding-p 'tag '(catch 'tag)))

  (should-not (erefactor--local-binding-p 'v '(defun f (v1) v)))
  (should-not (erefactor--local-binding-p 'v '(lambda (v1) v)))
  (should-not (erefactor--local-binding-p 'v '(let ((v1 val)) v)))
  (should-not (erefactor--local-binding-p 'tag '(catch 'tag1 tag))))

;; TODO comment out for now
(ert-deftest erefactor-local-bindings-in-macro ()
  :tags '(erefactor)

  ;; need to macro expand when execute time
  (require 'cl)

  (should (erefactor--macroexpand-contains-p 'v '(defun* f (v))))
  (should (erefactor--macroexpand-contains-p 'k1 '(defun* f (v1 &key k1))))
  (should-not (erefactor--macroexpand-contains-p 'v '(defun* f (v1) v)))

  ;; check ignoring failed (defface) form expansion
  (should-not (erefactor--macroexpand-contains-p 'v1 '(when test (case v1 (defface)))))

  ;; cannot test `erefactor--local-fbinding' because that move point.
  )



;; (macroexpand-all '(erefactor--macroexpand-contains-p 'v '(defun* f (v))))

(provide 'erefactor-test)
;;; erefactor-test.el ends here
