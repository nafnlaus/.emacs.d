;; Open init.el
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

;; Open keybinds.el
(defun find-user-keybinds-file ()
  "Edit the user's keybinds.el file, in another window."
  (interactive)
  (find-file-other-window "~/.emacs.d/keybinds.el"))

;; Open this file (functions.el)
(defun find-user-functions-file ()
  "Edit the user's functions.el file, in another window."
  (interactive)
  (find-file-other-window "~/.emacs.d/functions.el"))

;; Open packages.el
(defun find-user-packages-file ()
  "Edit the user's packagess.el file, in another window."
  (interactive)
  (find-file-other-window "~/.emacs.d/packages.el"))

;; Open ui.el
(defun find-user-ui-file ()
  "Edit the user's ui.el file, in another window."
  (interactive)
  (find-file-other-window "~/.emacs.d/ui.el"))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "New name: ")
  (let ((name (buffer-name))
    (filename (buffer-file-name)))
    (if (not filename)
    (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
      (message "A buffer named '%s' already exists!" new-name)
    (progn
      (rename-file filename new-name 1)
      (rename-buffer new-name)
      (set-visited-file-name new-name)
      (set-buffer-modified-p nil))))))

(provide 'functions)
;;; functions.el ends here
