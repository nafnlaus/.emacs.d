;;; init-leader-keys.el ---   -*- lexical-binding: t -*-

;; Author: Alex de Wit
;; Maintainer: Alex de Wit
;; Version: none
;; Package-Requires: ()
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

(defmacro general-with-eval-after-load (file &rest body)
  "Like `with-eval-after-load' but don't always add to `after-load-alist'.
When FILE has already been loaded, execute BODY immediately without adding it to
`after-load-alist'."
  (declare (indent 1)
           (debug t))
  `(if (if (stringp ,file)
           (load-history-filename-element
            (purecopy (load-history-regexp ,file)))
         (featurep ,file))
       (progn ,@body)
     (eval-after-load ,file (lambda () ,@body))))

(general-with-eval-after-load 'use-package-core
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

  (defun general--sanitize-arglist (arglist)
    "Remove positional/separator arguments from ARGLIST."
    (let ((arglists (if (eq (car arglist) 'general-defs)
                        (general--parse-defs-arglists (cdr arglist))
                      (list arglist))))
      (cl-loop for arglist in arglists
               do (while (general--positional-arg-p (car arglist))
                    (setq arglist (cdr arglist)))
               and append arglist)))

  ;; altered args will be passed to the autoloads and handler functions
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
                   (if (or (functionp (car arglist))
                           (macrop (car arglist)))
                       `(,@arglist :package ',name)
                     `(general-def
                        ,@arglist
                        :package ',name)))
                 (plist-get args :arglists))))))

(provide 'init-leader-keys)
;;; init-leader-keys.el ends here
