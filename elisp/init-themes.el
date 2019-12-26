;;; init-themes.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-themes.el
;; Description: initialize my Emacs :)
;; Author: Alex de Wit
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Configures themeing and modeline
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

(use-package all-the-icons :ensure t)

(use-package doom-themes :ensure t
 :config
 (setq doom-themes-enable-bold t
       doom-themes-enable-italic t)
 (load-theme 'doom-nord t)
 (doom-themes-visual-bell-config)
 (doom-themes-neotree-config)
 (doom-themes-treemacs-config))

(use-package doom-modeline :ensure t
  :init (fset 'battery-update #'ignore)
  :hook (after-init . doom-modeline-mode))

(use-package rainbow-delimiters :ensure t
  :hook ('prog-mode . rainbow-delimiters-mode))

(provide 'init-themes)
;;; init-themes.el ends here
