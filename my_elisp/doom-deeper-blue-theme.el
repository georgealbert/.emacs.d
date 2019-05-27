;;; doom-deeper-blue-theme.el --- inspired by Emacs built-in theme deeper-blue
(require 'doom-themes)

;; [2019-05-21 周二 16:21:10]
;;
;; 参考doom-themes-common.el
;;
;; TIP:
;;  1. 有问题的颜色可以先改成红色来查找
;;
;; TODO:
;;  1. isearch增量查询时的高亮颜色没法设置多个颜色 - ok
;;  2. org一级outline的背景色没法消除
;;  3. major/minor mode无法显示
;;  4. cursor的颜色现在是紫色的，deeper-blue-theme中是绿色的 - ok
;;  5. link的颜色变成紫色了，和原来的不一样
;;  6. 括号匹配时，另外一边显示得不够清楚，只是黑色的背景色 - ok
;;
;; 优点:
;;  1. org-mode中的列表和序号是有颜色的
;;  2. org-mode中 =有颜色=
;;  3. 行号可以高亮显示

;;
(defgroup doom-deeper-blue-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-deeper-blue-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-deeper-blue-theme
  :type 'boolean)

(defcustom doom-deeper-blue-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-deeper-blue-theme
  :type 'boolean)

(defcustom doom-deeper-blue-comment-bg doom-deeper-blue-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-deeper-blue-theme
  :type 'boolean)

(defcustom doom-deeper-blue-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-deeper-blue-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-deeper-blue
  "A dark theme inspired by Emacs builtin deeper-blue"

  ;; name        default   256       16
  ((bg         '("#181a26" "#1c1c1c" nil             )) ;; background from deeper-blue-theme.el
   (bg-alt     '("#181a26" nil       nil             ))
   (base0      '("#100e23" "black"   "black"         ))
   (base1      '("#292F37" "#1e1e1e" "brightblack"   ))
   (base2      '("#103050" "#2e2e2e" "brightblack"   )) ;; region(#103050)
   (base3      '("#4C4B68" "#262626" "brightblack"   ))
   (base4      '("gray50" "#3f3f3f" "brightblack"    )) ;; 用于行号的高亮显示，gray50(#7f7f7f)
   (base5      '("moccasin" "#525252" "brightblack"  )) ;; doc comments(函数的说明等)和modeline的fg颜色, moccasin(#ffe4b5)
   (base6      '("burlywood" "#6b6b6b" "brightblack" )) ;; base6没发现其他地方使用的，改为Burlywood(#deb887)
   (base7      '("#B0BED8" "#979797" "brightblack"   )) ;; 用于注释的颜色，这个颜色就很好，注释容易看清楚
   (base8      '("#BAC9E4" "#dfdfdf" "white"         )) ;; 在加亮modeline时，不确定是什么时候用的?
   (fg-alt     '("gray80" "#bfbfbf" "brightwhite"    )) ;; gray80(#cccccc)
   (fg         '("gray80" "#2d2d2d" "white"          )) ;; gray80 #cccccc foreground from deeper-blue-theme.el

   (grey       base4)
   (red        '("#FF0000" "#ff6655" "red"               )) ;; red(#FF0000), #e45649 doom-one-light
   (orange     '("gold" "#dd8844" "brightred"            )) ;; gold(#ffd700)
   (green      '("SeaGreen2" "#99bb66" "green"           )) ;; #98be65 from doom-one-theme.el, SeaGreen2(#4eee94)，#50a14f doom-one-light
   (teal       '("goldenrod" "#44b9b1" "brightgreen"     )) ;; goldenrod(#daa520), function-name 
   ;; (yellow     '("CadetBlue1" "#ECBE7B" "yellow"            )) ;; CadetBlue1(#98f5ff), warning 即flycheck的warning，还有日期框，原来用Yellow2(#eeee00)
   (yellow     '("#eeee00" "#ECBE7B" "yellow"            )) ;; CadetBlue1(#98f5ff), warning 即flycheck的warning，还有日期框，原来用Yellow2(#eeee00)
   (blue       '("DeepSkyBlue1" "#51afef" "brightblue"   )) ;; selection DeepSkyBlue1(#00bfff) or blue3(#0000cd)
   (dark-blue  '("DodgerBlue4" "#2257A0" "blue"          )) ;; 查询时的highlight, DodgerBlue4  #104e8b
   (magenta    '("LightCoral" "#c678dd" "magenta"        )) ;; LightCoral(#f08080)
   (violet     '("#EE82EE" "#a9a1e1" "brightmagenta"     ))
   (cyan       '("DarkOliveGreen3" "#46D9FF" "brightcyan")) ;; DarkOliveGreen3(#a2cd5a)
   (dark-cyan  '("#008B8B" "#5699AF" "cyan"              )) ;; dark-cyan(#008b8b)

   ;; face categories -- required for all themes
   (highlight      violet)
   (vertical-bar   base1) ;; window的分割线
   (selection      dark-cyan) ;; blue3(#0000cd)
   (builtin        magenta) ;; LightCoral(#f08080)
   (comments       (if doom-deeper-blue-brighter-comments base3 base7))
   (doc-comments   (if doom-deeper-blue-brighter-comments (doom-darken dark-cyan 0.3) base5))
   (constants      cyan) ;; DarkOliveGreen3(#a2cd5a) - ok, 即(require 'package的颜色)
   (functions      teal) ;; goldenrod, function-name 
   (keywords       blue) ;; DeepSkyBlue1
   (methods        magenta) ;; LightCoral
   (operators      orange) ;; gold(#ffd700, c mode中的 #include 的颜色, preprocessor-face
   (type           "CadetBlue1") ;; CadetBlue1(#98f5ff)
   (strings        base6) ;; Burlywood(#deb887)- ok
   (variables      "SeaGreen2") ;; SeaGreen2(#4eee94)
   (numbers        orange) ;; gold
   (region         base2) ;; #103050
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-deeper-blue-brighter-modeline)
   (-modeline-pad
    (when doom-deeper-blue-padded-modeline
      (if (integerp doom-deeper-blue-padded-modeline) doom-deeper-blue-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        base3
      `(,(doom-darken (car bg) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        base3
      `(,(doom-darken (car bg) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-deeper-blue-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

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

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-code-face :background (doom-lighten base3 0.05))

   ;; outline (affects org-mode)
   ((outline-1 &override) :foreground blue :background nil) ;; SkyBlue1
   ((outline-2 &override) :foreground "CadetBlue1") ;; CadetBlue1
   ((outline-3 &override) :foreground "LightSteelBlue1") ;; LightSteelBlue1(#cae1ff)
   ((outline-4 &override) :foreground "turquoise2") ;; turquoise2(#00e5ee)
   ((outline-5 &override) :foreground "aquamarine1") ;; aquamarine1(#7fffd4)
   ((outline-6 &override) :foreground (doom-darken blue 0.2))
   ((outline-7 &override) :foreground (doom-darken "CadetBlue1" 0.2))
   ((outline-8 &override) :foreground (doom-darken "LightSteelBlue1" 0.2))

   ;; org-mode
   ;; 前景色设为和背景色一样，这样就不是显示黑色的背景色
   (org-hide :foreground bg)
   ;; (org-hide :foreground hidden)
   ;; (org-hide :foreground red)

   ;; 黑色的背景色显得有点突兀
   ;; (org-block :background "black")
   ;; (org-block :background base2)
   ;; 和theme的背景色一样比较合适
   (org-block :background bg)

   ;; block的begin-line的背景色设置为黑色后有点刺眼
   ;; (org-block-begin-line :background "black" :foreground comments)
   (org-block-begin-line :background bg :foreground comments)

   (org-date :foreground "CadetBlue1")

   (solaire-org-hide-face :foreground hidden)

   ;; porting from deeper-blue
   ;; isearch and match
   (isearch :background "coral2" :foreground "white")
   (isearch-lazy-highlight-face :background "coral4" :foreground "white")
   (lazy-highlight :background "cadetblue" :foreground "white") ;; cadetblue #5f9ea0
   (match :background "DeepPink4")

   ;; paren match
   (show-paren-match :background "DodgerBlue1" :foreground "white")
   (show-paren-mismatch :background "red1" :foreground "white")

   ;; helm
   (helm-selection :background "DarkGreen" :distant-foreground "gold") ;; :background "darkgreen" 好像和 "green4" 差不多
   (helm-match :foreground "gold" :distant-foreground base8)

   (cursor :background "green")

   ;; (minibuffer-prompt :foreground "CadetBlue1")
   ;; (primary-selection :background "blue3")
   ;; (ido-first-match :weight normal :foreground "orange")
   ;; (ido-only-match :foreground "green")
   ;; (ido-subdir :foreground nil :inherit font-lock-keyword-face)

   ;; (fringe :background "black")
   ;; (highlight :background "DodgerBlue4")

   ;; mode-line-buffer-id 即 filename的显示
   (mode-line-buffer-id :weight normal :foreground blue) ;; DodgeBlue4

   ;; magit
   ;; (magit-diff-added :foreground "white" :background (doom-blend green bg 0.1))
   (magit-diff-added :foreground "white" :background "DarkSlateGray4")

   ;; (magit-diff-added-highlight :foreground "white" :background (doom-blend green bg 0.2) :weight 'bold)
   (magit-diff-added-highlight :foreground "white" :background "SeaGreen4")

   (magit-diff-removed :foreground "white" :background (doom-blend red base3 0.1))
   (magit-diff-removed-highlight :foreground "white" :background (doom-blend red base3 0.2))

   (magit-diff-context-highlight :foreground fg :background "gray30")
   )
   
  ;; --- extra variables ---------------------
  ;; ()

  )

;;; doom-deeper-blue-theme.el ends here
