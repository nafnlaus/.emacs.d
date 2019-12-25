;;; packages.el --- -*- lexical-binding: t -*-
;;
;; Filename: packages.el
;; Description: load and configure all packages
;; Author: Alex de Wit
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file loads and configures all packages for my Emacs config
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

;;; Install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-compute-statistics t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key)); Bootstrap `use-package'


;; ================ UTILITY STARTS HERE ===============
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7) ;; in days
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

(use-package gnu-elpa-keyring-update)

(use-package dired
  :ensure nil
  :custom
  ;; Always delete and copy recursively
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  ;; Auto refresh Dired, but be quiet about it
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  ;; Quickly copy/move file in Dired
  (dired-dwim-target t)
  ;; Move files to trash when deleting
  (delete-by-moving-to-trash t)
  :config
  ;; Reuse same dired buffer, to prevent numerous buffers while navigating in dired
  (put 'dired-find-alternate-file 'disabled nil))

(use-package shell-here
  :bind ("M-`" . shell-here)
  :config
  (if (eq 'system-type "gnu/linux")
    (setq explicit-shell-file-name "/bin/bash")))

;; ================ UTILITY ENDS HERE ===============

;; ================ UI STARTS HERE ===============
(use-package rainbow-delimiters :ensure t
  :hook ('prog-mode . rainbow-delimiters-mode))

(use-package all-the-icons :ensure t)

(use-package material-theme
  :disabled
  :ensure t)

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

(use-package which-key
  :diminish
  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+")
  :config
  (which-key-mode))


;; ================ UI ENDS HERE ===============

;; ================ EVIL-MODE CONFIG STARTS HERE ===============
(use-package evil
  :ensure t ;; install the evil package if not installed
  :init ;; tweak evil's configuration before loading it
  (setq evil-want-keybinding nil)
  (setq evil-want-integration nil)
  (setq evil-search-module 'swiper)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  :config ;; tweak evil after loading it
  (evil-mode))


(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))


(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-org :ensure t
  :disabled
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package eyebrowse
  :ensure t
  :init
  (evil-define-key nil evil-window-map
    "0" 'eyebrowse-switch-to-window-config-0
    "1" 'eyebrowse-switch-to-window-config-1
    "2" 'eyebrowse-switch-to-window-config-2
    "3" 'eyebrowse-switch-to-window-config-3
    "4" 'eyebrowse-switch-to-window-config-4
    "5" 'eyebrowse-switch-to-window-config-5
    "6" 'eyebrowse-switch-to-window-config-6
    "7" 'eyebrowse-switch-to-window-config-7
    "8" 'eyebrowse-switch-to-window-config-8
    "9" 'eyebrowse-switch-to-window-config-9
    "." 'eyebrowse-next-window-config
    "," 'eyebrowse-prev-window-config
    "'" 'eyebrowse-last-window-config
    "\\" 'eyebrowse-close-window-config))
;; ================ EVIL-MODE CONFIG ENDS HERE ===============


;; ================ PROGRAMMING PACKAGES START HERE ===============
(add-hook 'c++-mode-hook 'my-c-mode-config)
(add-hook 'c-mode-hook 'my-c-mode-config)

(use-package flycheck :ensure t
  :pin melpa-stable
  :init (global-flycheck-mode))

(use-package ivy :ensure t
  :config (ivy-mode 1))

(use-package swiper :ensure t)

(use-package counsel :ensure t
  :config (counsel-mode 1))

(use-package company :ensure t
  :hook (prog-mode . company-mode))

(use-package smartparens :ensure t
  :config
  (progn
    (smartparens-global-mode)
    (show-smartparens-global-mode t)))

(use-package modern-cpp-font-lock
  :diminish t
  :init (modern-c++-font-lock-global-mode t))


;; hy-mode
(use-package elpy :ensure t
  :hook (python-mode . elpy-mode)
  :init (elpy-enable))
  
(setq python-shell-interpreter "python3")

(use-package magit :ensure t
  :defer t)

(use-package evil-magit
  :ensure t
  :after evil magit)

(use-package forge :ensure t
  :defer t
  :after magit)
;; ESS
;; Racket??
;; Scala

(provide 'packages)
;;; packages.el ends here
