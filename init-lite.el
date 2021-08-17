;; [2021-08-16 Mon 09:46:47] 轻量级emacs配置，优化启动速度
;; cd /Applications/Emacs-28.0.50.app/Contents/MacOS
;; ./Emacs -Q --load ~/.emacs.d/init-lite.el

(setq load-prefer-newer noninteractive)

;; doom-emacs/core/core.el
(defconst EMACS27+   (> emacs-major-version 26))
(defconst EMACS28+   (> emacs-major-version 27))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;; (defun add-subdirs-to-load-path (&rest _)
;;   "Add subdirectories to `load-path'."
;;   (let ((default-directory
;;           (expand-file-name "/Users/albert/.emacs.d/elpa" user-emacs-directory)))
;;     (normal-top-level-add-subdirs-to-load-path)))

;; (push (expand-file-name "/Users/albert/.emacs.d/elpa" user-emacs-directory) load-path)

(push (expand-file-name "/Users/albert/.emacs.d/elpa/evil-20210628.1913" user-emacs-directory) load-path)
(push (expand-file-name "/Users/albert/.emacs.d/elpa/benchmark-init-20150905.938/" user-emacs-directory) load-path)

(require 'benchmark-init-modes)
(require 'benchmark-init)
(benchmark-init/activate)

;; (add-subdirs-to-load-path)

;; Contrary to what many Emacs users have in their configs, you don't need more
;; than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")

(prefer-coding-system 'utf-8)
;;
;;; Optimizations

;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly, from 0.5s:
(setq idle-update-delay 1)

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (don't rely on case insensitivity for file names).
(setq auto-mode-case-fold nil)

;; Disable bidirectional text rendering for a modest performance boost. Of
;; course, this renders Emacs unable to detect/display right-to-left languages
;; (sorry!), but for us left-to-right language speakers/writers, it's a boon.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate fontification immediately after scrolling.
(setq fast-but-imprecise-scrolling t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it there anyway, just in case. This increases
;; memory usage, however!
(setq inhibit-compacting-font-caches t)

;; Increase how much is read from processes in a single chunk (default is 4kb).
;; This is further increased elsewhere, where needed (like our LSP module).
(setq read-process-output-max (* 1024 1024))

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless IS-MAC (setq command-line-ns-option-alist nil))
;; (unless IS-LINUX (setq command-line-x-option-alist nil))

(setq default-directory "~/")


(setq initial-major-mode 'fundamental-mode)
;;
;;; ui

;; 去掉启动欢迎界面
(setq inhibit-startup-message t) 

;; 光标靠近鼠标的时候，让鼠标自动让开，不挡住视线
(mouse-avoidance-mode 'animate)

;; 不显示工具栏和滚动条
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

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

;;
;;; evil setting

(run-with-idle-timer
 1 nil
 #'(lambda ()

     (require 'evil)

     ;; (with-eval-after-load 'evil
     (setq evil-want-visual-char-semi-exclusive t
           evil-echo-state t
           evil-ex-search-vim-style-regexp t
           ;; foo-bar vim认为就是一个word，emacs会认为是2个
           evil-symbol-word-search t

           evil-undo-system 'undo-redo)

     (evil-mode 1)

     ;; 使用isearch时，增加search结束后的，高亮时间到30s
     (setq evil-flash-delay 30)
     ;; evil keymap
     (define-key evil-normal-state-map "go" 'goto-char)
     ))

;; adjust the size of Emacs window for org mode agenda/todo list to display herizontal
(if (or (eq system-type 'windows-nt) (eq system-type 'darwin))
    (toggle-frame-maximized))

(set-face-attribute 'default nil :font "UbuntuMono Nerd Font Mono 20")
