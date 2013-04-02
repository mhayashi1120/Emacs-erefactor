# erefactor.el

Simple refactoring, linting utilities for Emacs-Lisp.

## Install:

Put this file into load-path'ed directory,
and byte compile its if desired.
And put the following expression into your ~/.emacs.

    (require 'erefactor)
    (add-hook 'emacs-lisp-mode-hook
       (lambda ()
         (define-key emacs-lisp-mode-map "\C-c\C-v" erefactor-map)))

And set these variables correctly.
 `erefactor-lint-path-alist', `erefactor-lint-by-emacsen'

Put the following in your .emacs, if you desire highlighting local variable.

    (add-hook 'emacs-lisp-mode-hook 'erefactor-lazy-highlight-turn-on)
    (add-hook 'lisp-interaction-mode-hook 'erefactor-lazy-highlight-turn-on)

