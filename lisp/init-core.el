;;; init-core.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;
;;; Custom hooks

;; doom-start.el
(defcustom doom-first-input-hook ()
  "Transient hooks run before the first user input."
  :type 'hook
  :local 'permanent-local
  :group 'doom)

;; (setq url-proxy-services
;;    '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;      ("http" . "127.0.0.1:7890")
;;      ("https" . "127.0.0.1:7890")))

;; doom-emacs/core/core.el
(defconst EMACS27+   (> emacs-major-version 26))
(defconst EMACS28+   (> emacs-major-version 27))
(defconst EMACS29+   (> emacs-major-version 28))
(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(defvar doom-interactive-mode (not noninteractive)
  "If non-nil, Emacs is in interactive mode.")
  
;; 显示加载时间
(defvar doom-init-time 'nil
  "The time that emacs init took, in seconds, for Doom Emacs to initialize.")

(defun doom-display-benchmark-h()
  "Display a benchmark, showing number of packages and modules, and how quickly
they were loaded at startup."
  (message "Emacs loaded %s packages in %.03fs"
           (length package-activated-list)
           ;; (length load-path)
           (or doom-init-time
               (setq doom-init-time
                     (float-time (time-subtract (current-time) before-init-time))))))

;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly, from 0.5s:
(setq idle-update-delay 1)

;;
;;; Optimizations

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

;; [2020-02-03 周一 22:22:16]
;; emacs 字符串连接对数据越大越慢，lsp的completion一次接收大量的数据需要进行字符串连接，
;; emacs的process-filter 一次默认只传输4096个字节，如果数据量
;; 过大就要对数据合并之后再进行json decode 这就会慢到爆炸。
;; 从emacs27开始，可以修改read-process-output-max
;; 
;; Increase how much is read from processes in a single chunk (default is 4kb).
;; This is further increased elsewhere, where needed (like our LSP module).
(setq read-process-output-max (* 1024 1024))

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; Performance on Windows is considerably worse than elsewhere, especially if
;; WSL is involved. We'll need everything we can get.
(when IS-WINDOWS
  ;; Reduce the workload when doing file IO
  (setq w32-get-true-file-attributes nil)
  ; faster IPC
  (setq w32-pipe-read-delay 0)
  ; read more at a time (was 4K)
  (setq w32-pipe-buffer-size (* 64 1024)))

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless IS-MAC   (setq command-line-ns-option-alist nil))
(unless IS-LINUX (setq command-line-x-option-alist nil))
  
;; The GC introduces annoying pauses and stuttering into our Emacs experience,
;; so we use `gcmh' to stave off the GC while we're using Emacs, and provoke it
;; when it's idle. However, if the idle delay is too long, we run the risk of
;; runaway memory usage in busy sessions. If it's too low, then we may as well
;; not be using gcmh at all.
(setq gcmh-idle-delay 'auto  ; default is 15s
      gcmh-auto-idle-delay-factor 10
      gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb

;; doom-emacs用的hook是 window-setup-hook
(when doom-interactive-mode
  (add-hook 'window-setup-hook #'doom-display-benchmark-h 'append))

(setq load-prefer-newer t)

(add-to-list 'package-archives
             '("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/") t)
             ;; '("melpa" . "http://elpa.emacs-china.org/melpa/") t)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-always-ensure t)
(setq use-package-verbose t)

(eval-when-compile
  (require 'use-package))

;; use win key in windows
(when IS-WINDOWS
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)
  (w32-register-hot-key [s-])
  ;; (w32-register-hot-key [s-s])
  )
  
;;
;;; Reasonable defaults for interactive sessions

;; Disable warnings from legacy advice system. They aren't useful, and what can
;; we do about them, besides changing packages upstream?
(setq ad-redefinition-action 'accept)

;; Reduce debug output, well, unless we've asked for it.
(setq debug-on-error init-file-debug
      jka-compr-verbose init-file-debug)

;; Get rid of "For information about GNU Emacs..." message at startup, unless
;; we're in a daemon session where it'll say "Starting Emacs daemon." instead,
;; which isn't so bad.
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; *scratch* buffer改为fundamental mode，就不会在emacs启动后显示*scratch* buffer
;; 时就加载display-numbers-mode和hl-todo了。
;; 貌似Helm-lisp以前也没见到会加载，怎么也加载了呢?
(setq initial-major-mode 'fundamental-mode)

(setq default-directory "~/")

(prefer-coding-system 'utf-8)

(when IS-WINDOWS
  (setq file-name-coding-system 'gbk))

;; for linux terminal
(when IS-LINUX
  (progn
    (setq locale-coding-system 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    (set-selection-coding-system 'utf-8)))

(doom-run-hook-on 'doom-first-input-hook  '(pre-command-hook))

(when (not (fboundp 'igc-stats))
  (use-package gcmh
    :diminish
    :hook (after-init . gcmh-mode)))

(use-package general
  :defer t)

(provide 'init-core)
