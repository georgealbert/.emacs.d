;;; init-ui.el -*- lexical-binding: t; -*-

;;; ui

;; Frame title
(setq frame-title-format "Albert@%f")

;; 去掉启动欢迎界面
(setq inhibit-startup-message t) 

;; 光标靠近鼠标的时候，让鼠标自动让开，不挡住视线
(mouse-avoidance-mode 'animate)

;; 不显示工具栏和滚动条
;; 在early-init.el里已经disable了menubar、toolbar和scrollbars了。
;; (menu-bar-mode -1)
;; (tool-bar-mode -1)

;; core/core-ui.el

;;
;;; General UX

(setq ;; uniquify-buffer-name-style 'forward
      ;; no beeping or blinking please
      ring-bell-function #'ignore
      visible-bell nil)

;;
;;; Scrolling

;; [2020-03-01 周日 14:44:10] 感觉3月1日编译后的emacs，移动光标时，屏幕刷新特别晃眼。先注释掉。
(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered.
      scroll-conservatively 1001
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(5 ((shift) . 2))
      mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling

;; Remove hscroll-margin in shells, otherwise it causes jumpiness
;; (setq-hook '(eshell-mode-hook term-mode-hook) hscroll-margin 0)

;; (if (eq window-system 'w32)
;; (when IS-WINDOWS
;;   (scroll-bar-mode -1))
  
;;
;;; Cursor [2020-01-02 周四 13:53:49]
;; 参考doom-emacs

;; Don't blink the cursor, it's too distracting.
(blink-cursor-mode -1)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)

;;
;;; Minibuffer

;; Show current key-sequence in minibuffer, like vim does. Any feedback after
;; typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; It often displays so much information, even temporarily, that it is nice to give it some room to breath.
(setq resize-mini-windows t)
(setq max-mini-window-height 0.33)

;;
;;; Fringes

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like git-gutter and flycheck.
;; (setq indicate-buffer-boundaries nil
;;       indicate-empty-lines nil)

;; 不要总是没完没了的问yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; 字体放大缩小. from sacha chua
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Make window splitting more useful
;; Copied from http://www.reddit.com/r/emacs/comments/25v0eo/you_emacs_tips_and_tricks/chldury
(defun vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun hsplit-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(global-set-key (kbd "C-x 3") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 2") 'hsplit-last-buffer)

;; [2019-05-15 周三 15:33:55] emacs 26.2+可以用，显示速度比linum快很多。
(use-package display-line-numbers
  :ensure nil
  :defer t
  :hook (prog-mode . display-line-numbers-mode)
        (org-mode . albert-display-line-numbers)
  :config
  ;; Explicitly define a width to reduce the cost of on-the-fly computation
  (setq-default display-line-numbers-width 3)

  ;; Show absolute line numbers for narrowed regions to make it easier to tell the
  ;; buffer is narrowed, and where you are, exactly.
  (setq-default display-line-numbers-widen t)

  (defun albert-display-line-numbers ()
    "org-mode的文件，如果行数<=10000行就显示行号."
    (let ((num (line-number-at-pos (point-max))))
      (message "Opening %s, total lines: %d" (buffer-file-name) num)
      (if (< num 10000)
          (display-line-numbers-mode 1)))))

;;; font setting
(defun albert-notebook-font()
  "Config font on HP zhan66."
  (if (eq system-type 'windows-nt)
      (progn
        (set-face-attribute 'default nil :font "UbuntuMono Nerd Font Mono 20")

        ;; http://www.jinbuguo.com/gui/linux_fontconfig.html
        ;; http://mix-mplus-ipa.osdn.jp/migu/
        ;; (set-face-attribute 'default nil :font "Migu 1M 11")

        (setq face-font-rescale-alist '(("等距更纱黑体 T SC" . 1)))

        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family "等距更纱黑体 T SC"))))))

(defun albert-s2319-font()
  "Config font on dell s2319. 
   Ubuntu Mono 10 + Yahei 14 太小了
   Ubuntu Mono 12 + Yahei 16 比较合适
   "
  (if (eq system-type 'windows-nt)
      (progn
        ;; (set-face-attribute 'default nil :font "Ubuntu Mono 12")
        (set-face-attribute 'default nil :font "Fixedsys 12")
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
                (set-fontset-font (frame-parameter nil 'font)
                              charset
                              (font-spec :family "等距更纱黑体 T SC" :size 16))))))

(defun albert-x240-font()
  "Config font on x240."
  (if (eq system-type 'windows-nt)
    (progn
      (set-face-attribute 'default nil :font "Ubuntu Mono 12")
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
              (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family "Microsoft Yahei" :size 16))))))

(defun albert-adjust-font()
  (when IS-WINDOWS
    (progn
      ;; 根据显示器实际宽度(以毫米为单位)，显示字体.
      ;; DELL S2319HS 分辨率: 1920x1080, 屏幕尺寸: 509mm * 286mm
      ;; EIZO EV2451 分辨率: 1920x1080, 屏幕尺寸: 528mm * 297mm
      ;; (display-mm-height)
      (if (>= (display-mm-width) 509)
        (albert-s2319-font))

      (if (eq (display-mm-width) 277)
        (albert-x240-font))

      ;; 宽度在500mm的认为是笔记本?或者更加精确一点的方式来匹配不同的笔记本型号?
      ;; HP ZHAN66 309mm X 175mm
      ;; Thinkpad T430 4xxmm X 20xmm?
      (if (eq (display-mm-width) 309)
        (albert-notebook-font)))))
        
(if (and (display-graphic-p) IS-WINDOWS)
    (albert-adjust-font))

(defun albert-macos-notebook-font()
  "macbook air on 1680x1050 or 1440x900."
  (progn
    ;; mba 1680x1050 用20
    ;; mba 1440x900 用18

    ;; https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/UbuntuMono/Regular
    ;; (set-face-attribute 'default nil :font "UbuntuMono Nerd Font 18")

    ;; https://github.com/belluzj/fantasque-sans
    ;; https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/FantasqueSansMono
    ;; (set-face-attribute 'default nil :font "FantasqueSansMono Nerd Font 18")
    (set-face-attribute 'default nil :font "FantasqueSansMono Nerd Font Mono 18")

    ;; 2023.02.14
    ;; https://www.kreativekorp.com/software/fonts/fairfaxhd/
    ;; 可以和HarmonyOS Sans 1:1配合，但是中文字体有点大
    ;; (set-face-attribute 'default nil :font "Fairfax SM HD 18")
    ;; (set-face-attribute 'default nil :font "Fairfax Hax HD 18")

    ;; (set-face-attribute 'default nil :font "iMWritingMonoS Nerd Font 16")
    ;; (set-face-attribute 'default nil :font "Anonymice Nerd Font 16")

    ;; https://www.codingfont.com/
    ;; (set-face-attribute 'default nil :font "Red Hat Mono 18")

    ;; https://github.com/lxgw/LxgwWenKai/releases
    ;; (set-face-attribute 'default nil :font "霞鹜文楷等宽 18")

    ;; (set-face-attribute 'default nil :font "Iosevka Nerd Font Mono 18")

    ;; (set-face-attribute 'default nil :font "Monolisa NF 18")

    ;; 问题是中文太大了，不美观
    ;; FiraCode size=16, HarmonyOS Sans SC size=20; rescale=1.25
    ;; (set-face-attribute 'default nil :font "Fira Code Retina 16")
    
    ;; (set-face-attribute 'default nil :font "MesloLGS NF 18")
    ;; (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font 18")
    ;; (set-face-attribute 'default nil :font "DejaVuSansMono Nerd Font 18")
    ;; (set-face-attribute 'default nil :font "BlexMono Nerd Font 18")
    ;; (set-face-attribute 'default nil :font "LiterationMono Nerd Font 18")
    ;; (set-face-attribute 'default nil :font "SauceCodePro Nerd Font 18")
    ;; (set-face-attribute 'default nil :font "VictorMono Nerd Font 18")
    ;; (set-face-attribute 'default nil :font "SpaceMono Nerd Font 18")
    ;; (set-face-attribute 'default nil :font "NotoSansMono Nerd Font 18")
    ;; (set-face-attribute 'default nil :font "ProFontIIx Nerd Font 18")

    ;; 可以等宽，但是小写L和数字1不好区分
    ;; (set-face-attribute 'default nil :font "PragmataPro Mono 18")
    ;; 2023.04.02 找到了nerd font。
    ;; (set-face-attribute 'default nil :font "PragmataProMonoLiga Nerd Font 18")

    ;; (set-face-attribute 'default nil :font "Input Mono Narrow 18")
    ;; (set-face-attribute 'default nil :font "Input Mono 18")

    ;; https://tosche.net/fonts/comic-code 收费
    ;; https://github.com/your-local-developer/comic-mono-nerd-font 这里下载的，不等宽
    ;; https://dtinth.github.io/comic-mono-font/
    ;; (set-face-attribute 'default nil :font "ComicMono Nerd Font 18")
    
    ;; 设置中文字体和英文字体的缩放比例，用1:1即可。

    ;; 鸿蒙字体比更纱黑体显示更清晰
    ;; https://developer.harmonyos.com/cn/docs/design/des-guides/font-0000001157868583
    ;; (setq face-font-rescale-alist '(("HarmonyOS Sans SC" . 1)))

    (setq face-font-rescale-alist '(("霞鹜文楷等宽" . 1)))
    ;; (setq face-font-rescale-alist '(("等距更纱黑体 T SC" . 1)))

    ;; 系统自带的`苹方-简'比`更纱黑'的字体稍微大了一点
    ;; (setq face-font-rescale-alist '(("苹方-简" . 1)))

    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        ;; (font-spec :family "苹方-简")))
                        ;; (font-spec :family "等距更纱黑体 T SC"))
                        ;; (font-spec :family "霞鹜文楷等宽"))
                        (font-spec :family "LXGW WenKai"))
                        ;; (font-spec :family "HarmonyOS Sans SC" :style "Regular"))
      )))

(when IS-MAC
  (albert-macos-notebook-font)

  ;; 让emacs左右分屏，mba在默认分辨率下是157
  ;; (list split-height-threshold split-width-threshold)
  ;; (list (window-height) (window-width))
  (setq split-width-threshold 156)

  ;; disable anti-aliases
  ;; https://stackoverflow.com/questions/1279906/turn-off-anti-alias-for-font-in-emacs-23
  ;; (setq-default mac-allow-anti-aliasing nil)
  )

;; https://idiocy.org/emacs-fonts-and-fontsets.html
;; http://users.teilar.gr/~g1951d/Symbola.zip
(set-fontset-font t nil "Symbola" nil 'append)

(use-package winum
  :hook (after-init . winum-mode)
  :config
  (add-to-list 'winum-ignored-buffers "*sort-tab*")
  :init
  (setq winum-keymap
        (if (or IS-WINDOWS IS-LINUX)
            (let ((map (make-sparse-keymap)))
              ;; (define-key map (kbd "C-`") 'winum-select-window-by-number)
              ;; (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
              (define-key map (kbd "M-1") 'winum-select-window-1)
              (define-key map (kbd "M-2") 'winum-select-window-2)
              (define-key map (kbd "M-3") 'winum-select-window-3)
              (define-key map (kbd "M-4") 'winum-select-window-4)
              (define-key map (kbd "M-5") 'winum-select-window-5)
              map)
          (let ((map (make-sparse-keymap)))
            (define-key map (kbd "s-1") 'winum-select-window-1)
            (define-key map (kbd "s-2") 'winum-select-window-2)
            (define-key map (kbd "s-3") 'winum-select-window-3)
            (define-key map (kbd "s-4") 'winum-select-window-4)
            (define-key map (kbd "s-5") 'winum-select-window-5)
            map))))

;; [2023-01-06 Fri 23:54:33]
(use-package sort-tab
  :defer 2
  :disabled t
  :load-path "~/workspace/sort-tab"
  :config
  (sort-tab-mode 1))

;; From seagle0128/.emacs.d/lisp/init-window.el
;; Restore old window configurations
(use-package winner
  :ensure nil
  ;; :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*inferior-lisp*"
                                      ;; "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      ;; "*cvs*"
                                      "*Buffer List*"
                                      "*Ibuffer*"
                                      "*esh command on file*")))

;; doom-emacs/modules/ui/hl-todo
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

;; https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-ui.el#L435
(when EMACS28+
  (use-package composite
    :ensure nil
    :init (defvar composition-ligature-table (make-char-table nil))
    :hook (((prog-mode
             conf-mode nxml-mode markdown-mode)
            . (lambda () (setq-local composition-function-table composition-ligature-table))))
    :config
    ;; support ligatures, some toned down to prevent hang
    (let ((alist
           '((33  . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
             (35  . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
             (36  . ".\\(?:\\(>\\)>?\\)")
             (37  . ".\\(?:\\(%\\)%?\\)")
             (38  . ".\\(?:\\(&\\)&?\\)")
             (42  . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
             ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
             (43  . ".\\(?:\\([>]\\)>?\\)")
             ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
             (45  . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
             ;; (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
             (46  . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
             (47  . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
             ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
             (48  . ".\\(?:x[a-zA-Z]\\)")
             (58  . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
             (59  . ".\\(?:\\(;\\);?\\)")
             (60  . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
             (61  . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
             (62  . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
             (63  . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
             (91  . ".\\(?:\\(|\\)[]|]?\\)")
             ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
             (94  . ".\\(?:\\(=\\)=?\\)")
             (95  . ".\\(?:\\(|_\\|[_]\\)_?\\)")
             (119 . ".\\(?:\\(ww\\)w?\\)")
             (123 . ".\\(?:\\(|\\)[|}]?\\)")
             (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
             (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
      (dolist (char-regexp alist)
        (set-char-table-range composition-ligature-table (car char-regexp)
                              `([,(cdr char-regexp) 0 font-shape-gstring]))))
    (set-char-table-parent composition-ligature-table composition-function-table)))

(provide 'init-ui)
