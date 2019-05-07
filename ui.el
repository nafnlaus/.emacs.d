;; ========== Themes/UI ==================
;; (load-theme 'material t) ;; load material theme

;; Toggle menu bar
(global-set-key (kbd "<f12>") 'menu-bar-mode)

;; delete the selection with a keypress
(delete-selection-mode t)

;; UI
(setq-default
 display-line-numbers 'relative
 display-line-numbers-width 3
 display-line-numbers-widen t)
 (display-line-numbers-mode);; enable relative line numbers globally
(show-paren-mode 1) ;; Show matching parenthesis
(setq ring-bell-function 'ignore)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(column-number-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(blink-cursor-mode 0)
(global-font-lock-mode t)

;; make dired filesizes human-readable.
(setq-default dired-listing-switches "-alh")

;; scroll one line at a time (less "jumpy" than defaults)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time


;; nice font
 (set-face-attribute 'default nil :family "Fira Code" :height 105)


(defun fira-code-mode--make-alist (list)
  "Generate prettify-symbols alist from LIST."
  (let ((idx -1))
    (mapcar
     (lambda (s)
       (setq idx (1+ idx))
       (let* ((code (+ #Xe100 idx))
          (width (string-width s))
          (prefix ())
          (suffix '(?\s (Br . Br)))
          (n 1))
     (while (< n width)
       (setq prefix (append prefix '(?\s (Br . Bl))))
       (setq n (1+ n)))
     (cons s (append prefix suffix (list (decode-char 'ucs code))))))
     list)))

(defconst fira-code-mode--ligatures
  '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
    "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
    "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
    "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
    ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
    "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
    "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
    "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
    ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
    "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
    "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
    "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
    "x" ":" "+" "+" "*" "|="))

(defvar fira-code-mode--old-prettify-alist)


(defun fira-code-mode--enable ()
  "Enable Fira Code ligatures in current buffer."
  (setq-local fira-code-mode--old-prettify-alist prettify-symbols-alist)
  (setq-local prettify-symbols-alist (append (fira-code-mode--make-alist fira-code-mode--ligatures)
                                             fira-code-mode--old-prettify-alist))
  (prettify-symbols-mode t))

(defun fira-code-mode--disable ()
  "Disable Fira Code ligatures in current buffer."
  (setq-local prettify-symbols-alist fira-code-mode--old-prettify-alist)
  (prettify-symbols-mode -1))

(define-minor-mode fira-code-mode
  "Fira Code ligatures minor mode"
  :lighter " Fira Code"
  (setq-local prettify-symbols-unprettify-at-point 'right-edge)
  (if fira-code-mode
      (fira-code-mode--enable)
    (fira-code-mode--disable)))

(defun fira-code-mode--setup ()
  "Setup Fira Code Symbols"
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol"))

(setq prettify-symbols-unprettify-at-point t)

(set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")

(defconst fira-code-font-lock-symbols-alist
  (mapcar (lambda (s)
            (cons (car s) (decode-char 'ucs (car (cdr s)))))
          (list '("www" #Xe100)
                '("<--" #Xe153))))

(add-hook 'prog-mode-hook
          (lambda ()
            (dolist (alias fira-code-font-lock-symbols-alist)
              (push alias prettify-symbols-alist))
            (prettify-symbols-mode)))

(provide 'fira-code-mode)

(define-global-minor-mode my-global-fira-mode fira-code-mode
  (lambda ()
    (when (not (memq major-mode
                     (list 'mu4e-main-mode 'shell-mode)))
      (fira-code-mode))))

(my-global-fira-mode 0)

