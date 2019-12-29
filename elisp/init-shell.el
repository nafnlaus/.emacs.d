;;; init-shell.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-shell.el
;; Description: initialize my Emacs :)
;; Author: Alex de Wit
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file configures the various Emacs shells
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(use-package shell-here
  :bind ("M-`" . shell-here)
  :config
  (if (eq 'system-type "gnu/linux")
      (setq explicit-shell-file-name "/bin/bash")))

;; configure multiterm/multishell
;; write a function that renames shell buffer upon opening it so that names
;; do not conflict

(provide 'init-shell)
;;; init-shell.el ends here
