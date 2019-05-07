;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


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
 (load-theme 'doom-one t)
 (doom-themes-visual-bell-config)
 (doom-themes-neotree-config)
 (doom-themes-treemacs-config))

(use-package doom-modeline :ensure t
  :hook (after-init . doom-modeline-mode))
;; ================ UI ENDS HERE ===============

;; ================ EVIL-MODE CONFIG STARTS HERE ===============
(use-package evil
  :ensure t ;; install the evil package if not installed
  :init ;; tweak evil's configuration before loading it
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

;; ================ EVIL-MODE CONFIG ENDS HERE ===============


;; ================ PROGRAMMING PACKAGES START HERE ===============
(use-package flycheck :ensure t
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

(use-package multiple-cursors :ensure t
  :disabled)

;; hy-mode
(use-package elpy :ensure t)
  
(use-package magit :ensure t)
(use-package evil-magit :ensure t)
(use-package forge :ensure t
  :after magit)
;; ESS
;; SCALA
;; Racket??
;; Scala

(provide 'packages)
;;; packages.el ends here
