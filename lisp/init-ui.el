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
;; (setq ;; hscroll-margin 2
;;       ;; hscroll-step 1
;;       ;; scroll-conservatively 10
;;       ;; scroll-margin 0
;;       ;; scroll-preserve-screen-position t
;;       ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
;;       ;; for tall lines.
;;       ;; auto-window-vscroll nil
;;       ;; mouse
;;       mouse-wheel-scroll-amount '(5 ((shift) . 2))
;;       mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling

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

;; 不要总是没完没了的问yes or no
(fset 'yes-or-no-p 'y-or-n-p)

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
  (setq-default display-line-numbers-width-start 4)

  ;; Show absolute line numbers for narrowed regions makes it easier to tell the
  ;; buffer is narrowed, and where you are, exactly.
  ;; (setq-default display-line-numbers-widen t)

  (defun albert-display-line-numbers ()
    "org-mode的文件，如果行数<10000行就显示行号."
    (let ((num (line-number-at-pos (point-max))))
      (message "Opening %s, total lines: %d" (buffer-file-name) num)
      (if (< num 10000)
          (display-line-numbers-mode 1)))))

;;; font setting
(defun albert-notebook-font()
  "Config font on HP zhan66."
  (interactive)
  (if (eq system-type 'windows-nt)
    (progn
      ;; Setting English Font
      (set-face-attribute 'default nil :font "Ubuntu Mono 11")
      ;; (set-face-attribute 'default nil :font "等距更纱黑体 T SC 10")
      ;; (set-face-attribute 'default nil :font "Sarasa Term SC 10")

      ;; http://www.jinbuguo.com/gui/linux_fontconfig.html
      ;; http://mix-mplus-ipa.osdn.jp/migu/
      ;; (set-face-attribute 'default nil :font "Migu 1M 11")

      ;; (set-face-attribute 'default nil :font "terminus 14")

      ;; Fixedsys在笔记本上字体有点发虚
      ;; (set-face-attribute 'default nil :font "Fixedsys Excelsior 12")
      ;; Chinese Font
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
              (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family "等距更纱黑体 T SC" :size 22))))))
                            ;; (font-spec :family "Microsoft Yahei" :size 22))))))

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
        
(if (display-graphic-p)
    (albert-adjust-font))

;; adjust the size of Emacs window for org mode agenda/todo list to display herizontal
(if (eq system-type 'windows-nt)
    (toggle-frame-maximized))

(use-package winum
  :hook (after-init . winum-mode)
  :init
  (setq winum-keymap
        (let ((map (make-sparse-keymap)))
          ;; (define-key map (kbd "C-`") 'winum-select-window-by-number)
          ;; (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
          (define-key map (kbd "M-1") 'winum-select-window-1)
          (define-key map (kbd "M-2") 'winum-select-window-2)
          (define-key map (kbd "M-3") 'winum-select-window-3)
          (define-key map (kbd "M-4") 'winum-select-window-4)
          (define-key map (kbd "M-5") 'winum-select-window-5)
          ;; (define-key map (kbd "M-6") 'winum-select-window-6)
          ;; (define-key map (kbd "M-7") 'winum-select-window-7)
          ;; (define-key map (kbd "M-8") 'winum-select-window-8)
          map)))

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
