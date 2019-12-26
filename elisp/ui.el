;; ========== Themes/UI ==================

;; Toggle menu bar

;; delete the selection with a keypress
(delete-selection-mode t)

;; ========== UI ==================

;; enable relative line numbers globally
(setq-default
 display-line-numbers 'relative
 display-line-numbers-width 3
 display-line-numbers-widen t)
(display-line-numbers-mode)

;; Show matching parenthesis
(show-paren-mode 1)

;; remove useless ui
(setq ring-bell-function 'ignore)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(blink-cursor-mode 0)
(setq inhibit-splash-screen t)

;; add some helpful ui elements
(column-number-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-font-lock-mode t)

;; make dired filesizes human-readable.
(setq-default dired-listing-switches "-alh")

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; nice font
(set-face-attribute 'default nil
                    :family "SauceCodePro NF"
                    :height 110
                    :weight 'normal
                    :width 'normal)

(provide 'ui)
;;; ui.el ends here
