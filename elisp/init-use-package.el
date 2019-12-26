;;; init-use-package.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-use-package.el
;; Description: load and configure all packages
;; Author: Alex de Wit
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file initializes use-package, which is used to configure all
;; packages.
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

;; package.el initialization
(require 'package)

;; Add MELPA
(setq package-archives
      '(("elpa"         . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 0)
        ("elpa"         . 5)
        ("melpa"        . 10)))

(package-initialize)

;; all configuration should be done explicitly in this file tree
(setq custom-file (make-temp-file ""))
;;; Install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key)); Bootstrap `use-package'

(use-package diminish)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7) ;; in days
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

(use-package gnu-elpa-keyring-update)

(provide 'init-use-package)
;;; init-use-package.el ends here
