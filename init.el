;; =========== init.el starts here ============
;; MELPA setup
(require 'package)

;; Add MELPA
(setq package-archives
      '(("elpa"         . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 10)
        ("elpa"         . 5)
        ("melpa"        . 0)))

(package-initialize)

;; Get your custom-set-variables shit outta my init file
(setq custom-file (make-temp-file ""))

;; prepare to load scripts
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))

(defun load-user-file (file)
  "Load a FILE in current user's configuration directory."
  (interactive "f")
  (load-file (expand-file-name file user-init-dir)))


;; ========== Load other files ====================

(load-user-file "functions.el")
(load-user-file "ui.el")
(load-user-file "packages.el")
(load-user-file "keybinds.el")
(load-user-file "email.el")

;; ========== General ==================
;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)
(setq-default major-mode 'org-mode)
