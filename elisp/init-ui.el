;;; init-ui.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-ui.el
;; Description: configure global user interface.
;; Author: Alex de Wit
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file configures all user interface elements and removes unneeded ones
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

;; delete the selection with a keypress
(delete-selection-mode t)

;; enable relative line numbers globally
(setq-default display-line-numbers 'relative
	      display-line-numbers-width 3
	      display-line-numbers-widen t)

(display-line-numbers-mode)

;; Show matching parenthesis
(setq blink-matching-delay 0)
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

(provide 'init-ui)
;;; init-ui.el ends here
