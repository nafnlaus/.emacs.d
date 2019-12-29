;;; init.el --- -*- lexical-binding: t -*-
;;
;; Filename: init.el
;; Description: initialize my Emacs :)
;; Author: Alex de Wit
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This is the init.el file for my Emacs config
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

;; add elisp directory to load-path
(defun update-to-load-path (folder)
  "Update FOLDER and its subdirectories to `load-path'."
  (let ((base folder))
    (unless (member base load-path)
      (add-to-list 'load-path base))
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
        (when (and (file-directory-p name)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (unless (member base load-path)
            (add-to-list 'load-path name)))))))

(update-to-load-path (expand-file-name "elisp" user-emacs-directory))

;; ========== Load other files ====================

;;; initialize packages

;; bootstrap use-package
(require 'init-use-package)

;; ui packages
(require 'init-themes)
(require 'init-ui)

;; keybinding packages
(require 'init-evil)
(require 'init-eyebrowse)

;; misc
(require 'init-dired)
(require 'init-shell)
(require 'init-ivy)
(require 'init-magit)
(require 'init-save)
(require 'init-mu4e)

;; programming packages
(require 'init-company)
(require 'init-cc-mode)
(require 'init-flycheck)
(require 'init-smartparens)
;; (require 'init-quickrun)

;; custom keybinding maps
;; (require 'init-leader-maps)
;; (require 'init-evil-paren-state)


;; (require 'init-funtions) none right now

(provide 'init)
;;; init.el ends here
