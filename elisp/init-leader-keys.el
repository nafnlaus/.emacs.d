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
(defvar leader-compile-map (make-sparse-keymap)
  "Compile map under <leader>c")

;; template for the leader key definitions
(defvar leader-universal-commands
  `(("a" . x)
    ("b" . buffer-map)
;;  ("c" . ,leader-compile)
;;  ("d" . ,leader-jump-to-definition)
    ("e" . mu4e)
    ("f" . x)
    ("g" . magit)
;;  ("h" . x)
    ("i" . )
;;  ("j" . x)
;;  ("k" . x)
;;  ("l" . x)
    ("m" . x)
    ("n" . x)
    ("o" . x)
    ("p" . x)
    ("q" . x)
;;  ("r" . ,leader-run)
;;  ("s" . ,leader-search)
;;  ("t" . ,leader-test)
    ("u" . undo-tree-visualize)
    ("v" . x)
    ("w" . x)
    ("x" . x)
    ("y" . x)
    ("z" . x)
    ("`" . x)
    ("1" . x)
    ("2" . x)
    ("3" . x)
    ("4" . x)
    ("5" . x)
    ("6" . x)
    ("7" . x)
    ("8" . x)
    ("9" . x)
    ("0" . x)
    ("-" . x)
    ("=" . x)
    ("[" . x)
    ("]" . x)
    ("\\" . x)
    (";" . x)
    ("'" . x)
    ("," . x)
    ("." . x)
    ("/" . x)
    ;; shift modifier
    ("A" . x)
    ("B" . x)
    ("C" . x)
    ("D" . x)
    ("E" . x)
    ("F" . x)
    ("G" . x)
    ("H" . x)
    ("I" . x)
    ("J" . x)
    ("K" . x)
    ("L" . x)
    ("M" . x)
    ("N" . x)
    ("O" . x)
    ("P" . x)
    ("Q" . x)
    ("R" . x)
    ("S" . shell-here)
    ("T" . x)
    ("U" . x)
    ("V" . x)
    ("W" . x)
    ("X" . x)
    ("Y" . x)
    ("Z" . x)
    ("!" . shell-command)
    ("@" . x)
    ("#" . x)
    ("$" . x)
    ("%" . x)
    ("^" . x)
    ("&" . x)
    ("*" . x)
    ("(" . x)
    (")" . x)
    ("_" . x)
    ("+". x)
    ("{" . x)
    ("}" . x)
    ("|" . x)
    (":" . x)
    ("\"" . x)
    ("<" . x)
    (">" . x)
    ("?" . x))
  "Default alist for my leader mode.")

(defvar leader-major-mode-default-commands
  `(("a" . x)
    ("b" . x)
    ("c" . ,leader-compile)
    ("d" . x)
    ("e" . mu4e)
    ("f" . x)
    ("g" . magit)
    ("h" . x)
    ("i" . x)
    ("j" . ,leader-jump-to-definition)
    ("k" . x)
    ("l" . x)
    ("m" . x)
    ("n" . x)
    ("o" . x)
    ("p" . x)
    ("q" . x)
    ("r" . ,leader-run)
    ("s" . ,leader-search)
    ("t" . ,leader-test)
    ("u" . undo-tree-visualize)
    ("v" . x)
    ("w" . x)
    ("x" . x)
    ("y" . x)
    ("z" . x)
    ("[" . x)
    ("]" . x)
    ("\\" . x)
    (";" . x)
    ("'" . x)
    ("," . x)
    ("." . x)
    ("/" . x)
    ;; shift modifier
    ("A" . x)
    ("B" . x)
    ("C" . x)
    ("D" . x)
    ("E" . x)
    ("F" . x)
    ("G" . x)
    ("H" . x)
    ("I" . x)
    ("J" . x)
    ("K" . x)
    ("L" . x)
    ("M" . x)
    ("N" . x)
    ("O" . x)
    ("P" . x)
    ("Q" . x)
    ("R" . x)
    ("S" . shell)
    ("T" . x)
    ("U" . x)
    ("V" . x)
    ("W" . x)
    ("X" . x)
    ("Y" . x)
    ("Z" . x)
    ("{" . x)
    ("}" . x)
    ("|" . x)
    (":" . x)
    ("\"" . x)
    ("<" . x)
    (">" . x)
    ("?" . x))
  "Default alist for my leader mode.")


(with-eval-after-load 'use-package-core
  ;; step 1: introduce ryo-modal keyword before :bind
  (unless (member :leader use-package-keywords)
    (setq use-package-keywords (use-package-list-insert :leader use-package-keywords :bind)))

  ;; ensure deferred loading
  (when (boundp 'use-package-deferring-keywords)
    (add-to-list 'use-package-deferring-keywords :leader t))

  ;; step 2: normalize
  (defun use-package-normalize/:leader (_name _keyword args)
    "Apply lists of keywords to all keys following that list."
    (let (kwlist sanitized-args)
      (dolist (arg args sanitized-args)
        (cond
         ((symbolp (car arg))
          (setq kwlist arg))
         ((stringp (car arg))
          (push (append arg kwlist) sanitized-args))))))

  ;; step 2.5: autoload?

  ;; step 3: handler
  (defun use-package-handler/:leader (name _keyword arglists rest state)
    "Use-package handler for :leader."

    (use-package-concat
     (use-package-process-keywords name
       (use-package-sort-keywords
        (use-package-plist-append rest :commands
                                  (ryo-modal--extract-commands-from arglists)))
       state)
     `((ignore ,@(mapcar (lambda (arglist)
                           (if (stringp (cadr arglist))
                               `(ryo-modal-key ,(car arglist)
                                               ,(cadr arglist)
                                               ,@(nthcdr 2 arglist)
                                               :package ',name)
                             `(ryo-modal-key ,(car arglist)
                                             (quote ,(cadr arglist))
                                             ,@(nthcdr 2 arglist)
                                             :package ',name)))
                         arglists))))))




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
