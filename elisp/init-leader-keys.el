;;; init-leader-keys.el ---   -*- lexical-binding: t -*-

;; Author: Alex de Wit
;; Maintainer: Alex de Wit
;; Version: none
;; Package-Requires: ((cl-lib "0.5"))
;; Homepage: none
;; Keywords: keybindings


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file initializes my personal leader key config. It contains
;; default keymaps for various functions, as well as a macro and use-package
;; keyword that allow me to override the default functions bound to keys

;;; Code:

(defvar my-leader-map nil "The keymap for my leader keybindings.")

(defvar leader-jump-to-definition 'dumb-jump-other-window
  "Function to be used when jumping to other window.")
(defvar )

;; template for the leader key definitions
((setq my-leader-map (make-sparse-keymap))
       (evil-define-key 'normal my-leader-map "a" 'x)
       (evil-define-key 'normal my-leader-map "b" 'x)
       (evil-define-key 'normal my-leader-map "c" leader-compile)
       (evil-define-key 'normal my-leader-map "d" 'x)
       (evil-define-key 'normal my-leader-map "e" 'mu4e)
       (evil-define-key 'normal my-leader-map "f" 'x)
       (evil-define-key 'normal my-leader-map "g" 'x)
       (evil-define-key 'normal my-leader-map "h" 'x)
       (evil-define-key 'normal my-leader-map "i" 'x)
       (evil-define-key 'normal my-leader-map "j" leader-jump-to-definition)
       (evil-define-key 'normal my-leader-map "k" 'x)
       (evil-define-key 'normal my-leader-map "l" 'x)
       (evil-define-key 'normal my-leader-map "m" 'magit)
       (evil-define-key 'normal my-leader-map "n" 'x)
       (evil-define-key 'normal my-leader-map "o" 'x)
       (evil-define-key 'normal my-leader-map "p" 'x)
       (evil-define-key 'normal my-leader-map "q" 'x)
       (evil-define-key 'normal my-leader-map "r" leader-run)
       (evil-define-key 'normal my-leader-map "s" 'x)
       (evil-define-key 'normal my-leader-map "t" leader-test)
       (evil-define-key 'normal my-leader-map "u" 'x)
       (evil-define-key 'normal my-leader-map "v" 'x)
       (evil-define-key 'normal my-leader-map "w" 'x)
       (evil-define-key 'normal my-leader-map "x" 'x)
       (evil-define-key 'normal my-leader-map "y" 'x)
       (evil-define-key 'normal my-leader-map "z" 'x)
       (evil-define-key 'normal my-leader-map "[" 'x)
       (evil-define-key 'normal my-leader-map "]" 'x)
       (evil-define-key 'normal my-leader-map "\\" 'x)
       (evil-define-key 'normal my-leader-map ";" 'x)
       (evil-define-key 'normal my-leader-map "'" 'x)
       (evil-define-key 'normal my-leader-map "," 'x)
       (evil-define-key 'normal my-leader-map "." 'x)
       (evil-define-key 'normal my-leader-map "/" leader-search)

       (evil-define-key 'normal my-leader-map "A" 'x)
       (evil-define-key 'normal my-leader-map "B" 'x)
       (evil-define-key 'normal my-leader-map "C" 'x)
       (evil-define-key 'normal my-leader-map "D" 'x)
       (evil-define-key 'normal my-leader-map "E" 'x)
       (evil-define-key 'normal my-leader-map "F" 'x)
       (evil-define-key 'normal my-leader-map "G" 'x)
       (evil-define-key 'normal my-leader-map "H" 'x)
       (evil-define-key 'normal my-leader-map "I" 'x)
       (evil-define-key 'normal my-leader-map "J" 'x)
       (evil-define-key 'normal my-leader-map "K" 'x)
       (evil-define-key 'normal my-leader-map "L" 'x)
       (evil-define-key 'normal my-leader-map "M" 'x)
       (evil-define-key 'normal my-leader-map "N" 'x)
       (evil-define-key 'normal my-leader-map "O" 'x)
       (evil-define-key 'normal my-leader-map "P" 'x)
       (evil-define-key 'normal my-leader-map "Q" 'x)
       (evil-define-key 'normal my-leader-map "R" 'x)
       (evil-define-key 'normal my-leader-map "S" 'x)
       (evil-define-key 'normal my-leader-map "T" 'x)
       (evil-define-key 'normal my-leader-map "U" 'x)
       (evil-define-key 'normal my-leader-map "V" 'x)
       (evil-define-key 'normal my-leader-map "W" 'x)
       (evil-define-key 'normal my-leader-map "X" 'x)
       (evil-define-key 'normal my-leader-map "Y" 'x)
       (evil-define-key 'normal my-leader-map "Z" 'x)
       (evil-define-key 'normal my-leader-map "{" 'x)
       (evil-define-key 'normal my-leader-map "}" 'x)
       (evil-define-key 'normal my-leader-map "|" 'x)
       (evil-define-key 'normal my-leader-map ":" 'x)
       (evil-define-key 'normal my-leader-map "\"" 'x)
       (evil-define-key 'normal my-leader-map "<" 'x)
       (evil-define-key 'normal my-leader-map ">" 'x)
       (evil-define-key 'normal my-leader-map "?" 'x))

;; define use-package keyword :leader
(with-eval-after-load 'use-package-core
  (declare-function use-package-concat "use-package")
  (declare-function use-package-process-keywords "use-package")
  (defvar use-package-keywords)
  (defvar use-package-deferring-keywords)
  ;; ** :general Keyword
  (setq use-package-keywords
        ;; should go in the same location as :bind
        ;; adding to end may not cause problems, but see issue #22
        (cl-loop for item in use-package-keywords
                 if (eq item :bind-keymap*)
                 collect :bind-keymap* and collect :leader
                 else
                 ;; don't add duplicates
                 unless (eq item :leader)
                 collect item))

(defun use-package-normalize/:general (_name _keyword general-arglists)
    "Return a plist containing the original ARGLISTS and autoloadable symbols."
    (let* ((sanitized-arglist
            ;; combine arglists into one without function names or
            ;; positional arguments
            (cl-loop for arglist in general-arglists
                     append (general--sanitize-arglist arglist)))
           (commands
            (cl-loop for (key def) on sanitized-arglist by 'cddr
                     when (and (not (keywordp key))
                               (not (null def))
                               (ignore-errors
                                 ;; remove extra quote
                                 ;; `eval' works in some cases that `cadr' does
                                 ;; not (e.g. quoted string, '(list ...), etc.)
                                 ;; `ignore-errors' handles cases where it fails
                                 ;; (e.g. variable not defined at
                                 ;; macro-expansion time)
                                 (setq def (eval def))
                                 (setq def (general--extract-autoloadable-symbol
                                            def))))
                     collect def)))
      (list :arglists general-arglists :commands commands)))

  (defun use-package-autoloads/:general (_name _keyword args)
    "Return an alist of commands extracted from ARGS.
Return something like '((some-command-to-autoload . command) ...)."
    (mapcar (lambda (command) (cons command 'command))
            (plist-get args :commands)))

  (defun use-package-handler/:general (name _keyword args rest state)
    "Use-package handler for :general."
    (use-package-concat
     (use-package-process-keywords name rest state)
     `(,@(mapcar (lambda (arglist)
                   ;; Note: prefix commands are not valid functions
		   ;; if it is a custom definer function
                   (if (or (functionp (car arglist))
                           (macrop (car arglist)))
                       `(,@arglist :package ',name)
		     ;; if it is a list of keybindings
                     `(general-def
                        ,@arglist
                        :package ',name)))
		 ;; evaluate the lambda
                 (plist-get args :arglists))))))



(provide 'init-leader-keys)
;;; init-leader-keys.el ends here
