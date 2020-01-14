;;; doom-moonlit-theme.el --- inspired by my wallpaper -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-moonlit-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-moonlit-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-moonlit-theme
  :type 'boolean)

(defcustom doom-moonlit-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-moonlit-theme
  :type 'boolean)

(defcustom doom-moonlit-comment-bg doom-moonlit-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-moonlit-theme
  :type 'boolean)

(defcustom doom-moonlit-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-moonlit-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-moonlit
  "A dark theme inspired by Atom Moonlit Dark"

  ;; name        default   256       16
  ((bg         '("#0c1216" nil       nil            ))
   (bg-alt     '("#060a0c" nil       nil            ))
   (base0      '("#101b1f" "black"   "black"        ))
   (base1      '("#111c22" "#1e1e1e" "brightblack"  ))
   (base2      '("#101f24" "#2e2e2e" "brightblack"  ))
   (base3      '("#24292d" "#262626" "brightblack"  ))
   (base4      '("#283238" "#3f3f3f" "brightblack"  ))
   (base5      '("#1a2a33" "#525252" "brightblack"  ))
   (base6      '("#6d7073" "#6b6b6b" "brightblack"  ))
   (base7      '("#b4b5b6" "#979797" "brightblack"  ))
   (base8      '("#e6e6e6" "#dfdfdf" "white"        ))
   (fg         '("#e6e7e7" "#bfbfbf" "brightwhite"  ))
   (fg-alt     '("#9da0a1" "#2d2d2d" "white"        ))

   (grey       base4)
   (red        '("#e35c63" "#ff6655" "red"          ))
   (orange     '("#f0a46d" "#dd8844" "brightred"    ))
   (green      '("#b1c69f" "#99bb66" "green"        ))
   (teal       '("#03c9a8" "#44b9b1" "brightgreen"  ))
   (yellow     '("#cab36e" "#ECBE7B" "yellow"       ))
   (blue       '("#1cb2fc" "#51afef" "brightblue"   ))
   (dark-blue  '("#276381" "#2257A0" "blue"         ))
   (magenta    '("#825a79" "#c678dd" "brightmagenta"))
   (violet     '("#655783" "#a9a1e1" "magenta"      ))
   (cyan       '("#98dfe1" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#6a9c9d" "#5699AF" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      dark-blue)
   (vertical-bar   base1)
   (selection      dark-blue)
   (builtin        magenta)
   (comments       base6)
   (doc-comments   (doom-lighten base5 0.25))
   (constants      violet)
   (functions      magenta)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      magenta)
   (numbers        orange)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-moonlit-brighter-modeline)
   (-modeline-pad
    (when doom-moonlit-padded-modeline
      (if (integerp doom-moonlit-padded-modeline) doom-moonlit-padded-modeline 4)))

   (modeline-fg     fg)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.475)
      `(,(doom-darken (car bg) 0) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(doom-darken (car bg) 0) ,@(cdr bg-alt)))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-moonlit-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; Doom modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)

   ;; ivy-mode
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))

   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden))


  ;; --- extra variables ---------------------
  ()
  )

;;; doom-moonlit-theme.el ends here
