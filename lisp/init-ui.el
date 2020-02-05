;;; init-ui.el -*- lexical-binding: t; -*-

;;; ui

;; Frame title
(setq frame-title-format "Albert@%f")

;; 去掉启动欢迎界面
(setq inhibit-startup-message t) 

;; 光标靠近鼠标的时候，让鼠标自动让开，不挡住视线
(mouse-avoidance-mode 'animate)

;; 不显示工具栏和滚动条
(menu-bar-mode -1)
(tool-bar-mode -1)

;; core/core-ui.el

;;
;;; General UX

(setq ;; uniquify-buffer-name-style 'forward
      ;; no beeping or blinking please
      ring-bell-function #'ignore
      visible-bell nil)

;;
;;; Scrolling

(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 10
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
(when IS-WINDOWS
  (scroll-bar-mode -1))
  
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

;; 不要总是没完没了的问yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;;;###package paren
;; 显示匹配的括号
(use-package paren
  :ensure nil
  :disabled t
  :defer 2
  ;; highlight matching delimiters
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)
  (show-paren-mode +1))
  
(use-package smartparens
  ;; Auto-close delimiters and blocks as you type. It's more powerful than that,
  ;; but that is all Doom uses it for.
  ;;:after-call doom-switch-buffer-hook after-find-file
  ;; :hook (after-init . show-paren-mode)
  :defer 2
  :diminish smartparens-mode
  :commands sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string
  :config
  ;; Load default smartparens rules for various languages
  (require 'smartparens-config)

  ;; Overlays are too distracting and not terribly helpful. show-parens does
  ;; this for us already (and is faster), so...
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  (with-eval-after-load 'evil
    ;; But if someone does want overlays enabled, evil users will be stricken
    ;; with an off-by-one issue where smartparens assumes you're outside the
    ;; pair when you're really at the last character in insert mode. We must
    ;; correct this vile injustice.
    (setq sp-show-pair-from-inside t)
    ;; ...and stay highlighted until we've truly escaped the pair!
    (setq sp-cancel-autoskip-on-backward-movement nil))

  ;; The default is 100, because smartparen's scans are relatively expensive
  ;; (especially with large pair lists for somoe modes), we halve it, as a
  ;; better compromise between performance and accuracy.
  (setq sp-max-prefix-length 50)
  ;; This speeds up smartparens. No pair has any business being longer than 4
  ;; characters; if they must, set it buffer-locally.
  (setq sp-max-pair-length 4)
  ;; This isn't always smart enough to determine when we're in a string or not.
  ;; See https://github.com/Fuco1/smartparens/issues/783.
  (setq sp-escape-quotes-after-insert nil)

  ;; Silence some harmless but annoying echo-area spam
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (alist-get key sp-message-alist) nil))

  (add-hook 'minibuffer-setup-hook
    (defun doom-init-smartparens-in-minibuffer-maybe-h ()
      "Enable `smartparens-mode' in the minibuffer, during `eval-expression' or
`evil-ex'."
      (when (memq this-command '(eval-expression evil-ex))
        (smartparens-mode))))

  ;; You're likely writing lisp in the minibuffer, therefore, disable these
  ;; quote pairs, which lisps doesn't use for strings:
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'minibuffer-inactive-mode "`" nil :actions nil)

  ;; Smartparens breaks evil-mode's replace state
  (defvar doom-buffer-smartparens-mode nil)
  (add-hook 'evil-replace-state-exit-hook
    (defun doom-enable-smartparens-mode-maybe-h ()
      (when doom-buffer-smartparens-mode
        (turn-on-smartparens-mode)
        (kill-local-variable 'doom-buffer-smartparens-mode))))
  (add-hook 'evil-replace-state-entry-hook
    (defun doom-disable-smartparens-mode-maybe-h ()
      (when smartparens-mode
        (setq-local doom-buffer-smartparens-mode t)
        (turn-off-smartparens-mode))))
  (show-smartparens-global-mode +1)
  (smartparens-global-mode +1))


;; http://www.emacswiki.org/emacs/HighlightParentheses
;; http://ergoemacs.org/emacs/emacs_editing_lisp.html

(use-package highlight-parentheses
  :defer 2
  ;; :init (global-highlight-parentheses-mode +1) 
  :hook (prog-mode . highlight-parentheses-mode)
  ;; :diminish highlight-parentheses-mode
  :config
  (setq hl-paren-colors
    ;; 从左到右，依次是从内到外的括号的颜色
    '("firebrick1" "green" "purple" "DeepPink" "DeepSkyBlue" "violet" "IndianRed1" "IndianRed3" "IndianRed4")
    ))
;; 字体放大缩小. from sacha chua
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;;
;; Make window splitting more useful
;;

;; I added these snippets to my .emacs so that when I split the screen with C-x 2 or C-x 3, 
;; it opens the previous buffer instead of giving me two panes with the same buffer:

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

;; 显示行号
;; [2019-05-15 周三 15:33:55] emacs 26.2+可以用，显示速度比linum快很多。
(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode)
        (org-mode . albert-display-line-numbers)
  :init
  ;; 文件超过10000行，不显示行号，只留4位吧
  (setq display-line-numbers-width-start 4)

  (defun albert-display-line-numbers ()
    "org-mode的文件，如果行数<10000行就显示行号."
    (let ((num (line-number-at-pos (point-max))))
      (message "Opening %s, total lines: %d" (buffer-file-name) num)
      (if (< num 10000)
          (display-line-numbers-mode 1))))
  )

;; 我的doom-deeper-blue-theme.el在 ~/.emacs.d/my_elisp 目录中
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/my_elisp"))

(use-package doom-themes
  :ensure t
  :config
  (progn
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold nil  ; if nil, bold is universally disabled
                                       ; 禁用粗体，否则org-mode的outline字体太难看
          doom-themes-enable-italic t) ; if nil, italics is universally disabled

    ;; 在load-theme之前设置，让modeline更亮一点，
    (setq doom-deeper-blue-brighter-modeline nil)

    ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme may have their own settings.
    (load-theme 'doom-deeper-blue t)
    
    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config)
))

;; (use-package all-the-icons
;;   :ensure t
;;   :defer t)
  
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :init
  (unless after-init-time
    ;; prevent flash of unstyled modeline at startup
    (setq-default mode-line-format nil))

  ;; Whether show `all-the-icons' or not (if nil nothing will be showed).
  (setq doom-modeline-icon t)
  
  ;; Whether show the icon for major mode. It respects `doom-modeline-icon'.
  (setq doom-modeline-major-mode-icon t)
  
  ;; Display color icons for `major-mode'. It respects `all-the-icons-color-icons'.
  (setq doom-modeline-major-mode-color-icon t)
  
  ;; Whether display minor modes or not. Non-nil to display in mode-line.
  (setq doom-modeline-minor-modes nil)
  
  ;; Slow Rendering. If you experience a slow down in performace when rendering multiple icons simultaneously, you can try setting the following variable
  (setq inhibit-compacting-font-caches t)
  
  ;; Whether display `lsp' state or not. Non-nil to display in mode-line.
  (setq doom-modeline-lsp nil)
  
  ;; Whether display mu4e notifications or not. Requires `mu4e-alert' package.
  (setq doom-modeline-mu4e nil)

  (setq doom-modeline-github nil)
  
  (setq doom-modeline-persp-name nil)

  ;; Whether display irc notifications or not. Requires `circe' package.
  (setq doom-modeline-irc nil)
  
  ;; 2019.11.06修改为图标的了，不好看
  (setq doom-modeline-evil-state-icon nil)
  
  ;; 2019.11.22又改成下面这个变量了
  (setq doom-modeline-modal-icon nil)

  ;; [2020-01-05 周日 21:56:42] 从find-file-hook看见有hook，去掉
  (setq doom-modeline-persp-name nil)

  :config
  ;; 列号是从0开始的。
  (column-number-mode +1)
  
  (size-indication-mode +1) ; filesize in modeline
  )

;; modeline中的时间格式设置 [2014-11-21 周五 10:35:59]

;; (setq display-time-24hr-format t)
;; (setq display-time-use-mail-icon t)
;; (setq display-time-interval 60)

(setq display-time-day-and-date t)
(setq display-time-format "%Y-%m-%d %a %H:%M")
(setq display-time-default-load-average nil)
(display-time)

(defun albert-notebook-font()
  "Config font on HP zhan66."
  (interactive)
  (if (eq system-type 'windows-nt)
    (progn
      ;; Setting English Font
      (set-face-attribute 'default nil :font "Ubuntu Mono 11")
      ;; (set-face-attribute 'default nil :font "等距更纱黑体 T SC 10")
      ;; (set-face-attribute 'default nil :font "Sarasa Term SC 10")
      ;; Fixedsys在笔记本上字体有点发虚
      ;; (set-face-attribute 'default nil :font "Fixedsys Excelsior 12")
      ;; Chinese Font
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
              (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family "Microsoft Yahei" :size 22))))))

(defun albert-s2319-font()
  "Config font on dell s2319. 
   Ubuntu Mono 10 + Yahei 14 太小了
   Ubuntu Mono 12 + Yahei 16 比较合适
   "
  (interactive)
  (if (eq system-type 'windows-nt)
    (progn
      ;; Setting English Font
      ;; (set-face-attribute 'default nil :font "Ubuntu Mono 12")
      ;; (set-face-attribute 'default nil :font "Fixedsys Excelsior 12")
      (set-face-attribute 'default nil :font "Fixedsys 12")
      ;; Chinese Font
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
              (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family "Microsoft Yahei" :size 16))))))

(defun albert-x240-font()
  "Config font on x240."
  (interactive)
  (if (eq system-type 'windows-nt)
    (progn
      (set-face-attribute 'default nil :font "Ubuntu Mono 12")
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
              (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family "Microsoft Yahei" :size 16))))))

(defun albert-adjust-font()
  (interactive)
  ;;(if (eq window-system 'w32)
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
        
(albert-adjust-font)

;; adjust the size of Emacs window for org mode agenda/todo list to display herizontal
(if (eq system-type 'windows-nt)
    (toggle-frame-maximized))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if (executable-find "python3") 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      nil
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         30)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

;; (use-package treemacs-projectile
;;   :after treemacs projectile
;;   :disabled t
;;   :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :disabled t
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :disabled t
  :ensure t)

(use-package winum
  ;;:defer 3
  :hook (after-init . winum-mode)
  ;;:config (winum-mode +1)
  :init
  (setq winum-keymap
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "C-`") 'winum-select-window-by-number)
          (define-key map (kbd "C-²") 'winum-select-window-by-number)
          (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
          (define-key map (kbd "M-1") 'winum-select-window-1)
          (define-key map (kbd "M-2") 'winum-select-window-2)
          (define-key map (kbd "M-3") 'winum-select-window-3)
          (define-key map (kbd "M-4") 'winum-select-window-4)
          (define-key map (kbd "M-5") 'winum-select-window-5)
          (define-key map (kbd "M-6") 'winum-select-window-6)
          (define-key map (kbd "M-7") 'winum-select-window-7)
          (define-key map (kbd "M-8") 'winum-select-window-8)
          map)))

;; From seagle0128/.emacs.d/lisp/init-window.el
;; Restore old window configurations
(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*inferior-lisp*"
                                      "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      "*cvs*"
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
          ("DEPRECATED" font-lock-doc-face bold)))

  ;; Use a more primitive todo-keyword detection method in major modes that
  ;; don't use/have a valid syntax table entry for comments.
  ;; (add-hook '(pug-mode-hook haml-mode-hook)
  ;;   (defun +hl-todo--use-face-detection-h ()
  ;;     "Use a different, more primitive method of locating todo keywords."
  ;;     (set (make-local-variable 'hl-todo-keywords)
  ;;          '(((lambda (limit)
  ;;               (let (case-fold-search)
  ;;                 (and (re-search-forward hl-todo-regexp limit t)
  ;;                      (memq 'font-lock-comment-face (doom-enlist (get-text-property (point) 'face))))))
  ;;             (1 (hl-todo-get-face) t t))))
  ;;     (when hl-todo-mode
  ;;       (hl-todo-mode -1)
  ;;       (hl-todo-mode +1))))
)

(provide 'init-ui)
