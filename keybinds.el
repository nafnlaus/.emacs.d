;; =========== keybinds.el starts here ============
;; evil-leader and such for different modes

;; evil-surround takes over substitute maps
(evil-define-key 'normal evil-surround-mode-map "s" 'evil-surround-edit)
(evil-define-key 'normal evil-surround-mode-map "S" 'evil-Surround-edit)

(evil-define-key 'visual evil-surround-mode-map "S" 'evil-surround-region)
(evil-define-key 'visual evil-surround-mode-map "gS" 'evil-Surround-region)
