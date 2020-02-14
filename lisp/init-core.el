;;; init-core.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; doom-emacs/core/core-lib.el
;; (defmacro delq! (elt list &optional fetcher)
;;   "`delq' ELT from LIST in-place.

;; If FETCHER is a function, ELT is used as the key in LIST (an alist)."
;;   `(setq ,list
;;          (delq ,(if fetcher
;;                     `(funcall ,fetcher ,elt ,list)
;;                   elt)
;;                ,list)))

(defmacro add-transient-hook! (hook-or-function &rest forms)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.

FORMS are evaluated once, when that function/hook is first invoked, then never
again.

HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted function (which will be
advised)."
  (declare (indent 1))
  (let ((append (if (eq (car forms) :after) (pop forms)))
        (fn (intern (format "doom--transient-%s-h" (sxhash hook-or-function)))))
    `(let ((sym ,hook-or-function))
       (defun ,fn (&rest _)
         ,@forms
         (let ((sym ,hook-or-function))
           (cond ((functionp sym) (advice-remove sym #',fn))
                 ((symbolp sym)   (remove-hook sym #',fn))))
         (unintern ',fn nil))
       (cond ((functionp sym)
              (advice-add ,hook-or-function ,(if append :after :before) #',fn))
             ((symbolp sym)
              (put ',fn 'permanent-local-hook t)
              (add-hook sym #',fn ,append))))))

;; doom-emacs/core/core.el
(defconst EMACS27+   (> emacs-major-version 26))
(defconst EMACS28+   (> emacs-major-version 27))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(defvar doom-interactive-mode (not noninteractive)
  "If non-nil, Emacs is in interactive mode.")
  
(defvar doom--initial-file-name-handler-alist file-name-handler-alist)

;; This is consulted on every `require', `load' and various path/io functions.
;; You get a minor speed up by nooping this.
(setq file-name-handler-alist nil)

;; Restore `file-name-handler-alist', because it is needed for handling
;; encrypted or compressed files, among other things.
(defun doom-reset-file-handler-alist-h ()
  (setq file-name-handler-alist doom--initial-file-name-handler-alist))
(add-hook 'emacs-startup-hook #'doom-reset-file-handler-alist-h)

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

;; Performance on Windows is considerably worse than elsewhere, especially if
;; WSL is involved. We'll need everything we can get.
(when IS-WINDOWS
  ;; Reduce the workload when doing file IO
  (setq w32-get-true-file-attributes nil)

  ;; Font compacting can be terribly expensive, especially for rendering icon
  ;; fonts on Windows. Whether it has a noteable affect on Linux and Mac hasn't
  ;; been determined.
  (setq inhibit-compacting-font-caches t))
  
;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
;; (unless IS-MAC   (setq command-line-ns-option-alist nil))
(unless IS-LINUX (setq command-line-x-option-alist nil))
  
;; Adopt a sneaky garbage collection strategy of waiting until idle time to
;; collect; staving off the collector while the user is working.
(when doom-interactive-mode
  (add-transient-hook! 'pre-command-hook (gcmh-mode +1))
  (with-eval-after-load 'gcmh
    (setq gcmh-idle-delay 10
          gcmh-verbose nil)
    (add-hook 'focus-out-hook #'gcmh-idle-garbage-collect)))
    
;; doom-emacs用的hook是 window-setup-hook
(when doom-interactive-mode
  (add-hook 'window-setup-hook #'doom-display-benchmark-h 'append))

(setq load-prefer-newer t)

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://elpa.emacs-china.org/melpa/") t)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(setq use-package-always-ensure t)
(setq use-package-verbose t)
(require 'use-package)

;; use win key in windows
(when IS-WINDOWS
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)
  (w32-register-hot-key [s-])
  ;; (w32-register-hot-key [s-s])
  )
  
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

(use-package general
  ;; :init
  ;; Convenience aliases
  ;; (defalias 'define-key! #'general-def)
  ;; (defalias 'unmap! #'general-unbind)
  ;; :config
  ;; (general-evil-setup t)
  )


(provide 'init-core)
