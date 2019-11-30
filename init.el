;; =========== init.el starts here ============
;; MELPA setup
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

;; Get your custom-set-variables shit outta my init file
(setq custom-file (make-temp-file ""))

;; prepare to load other init files
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
;; (load-user-file "email.el")

;; ========== General ==================
;; Use Org as default mode
(setq-default major-mode 'org-mode)

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)
(make-directory "~/.emacs.d/autosaves" t)
(make-directory "~/.emacs.d/backups" t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/"))
      auto-save-file-name-transforms  '((".*" "~/.emacs.d/autosaves/\\1" t))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

;; Load the newest version of a file
(setq load-prefer-newer t)

;; Detect external file changes and auto refresh file
(global-auto-revert-mode t)

;; Transparently open compressed files
(auto-compression-mode t)

(provide 'init)
;;; init.el ends here
