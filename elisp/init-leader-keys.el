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
