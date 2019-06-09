;;; init.el -*- lexical-binding: t; -*-

;; This sets up the load path so that we can override it

;; 启动优化
(defvar doom-gc-cons-threshold 16777216 ; 16mb
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.")

(defvar doom-gc-cons-upper-limit 536870912 ; 512mb
  "The temporary value for `gc-cons-threshold' to defer it.")

(defvar doom--file-name-handler-alist file-name-handler-alist)

(defun doom|restore-startup-optimizations ()
  "Resets garbage collection settings to reasonable defaults (a large
`gc-cons-threshold' can cause random freezes otherwise) and resets
`file-name-handler-alist'."
  (setq file-name-handler-alist doom--file-name-handler-alist)
  ;; Do this on idle timer to defer a possible GC pause that could result; also
  ;; allows deferred packages to take advantage of these optimizations.
  (run-with-idle-timer
   3 nil
   (lambda ()
     (setq-default gc-cons-threshold doom-gc-cons-threshold)
     ;; To speed up minibuffer commands (like helm and ivy), we defer garbage
     ;; collection while the minibuffer is active.
     (defun doom|defer-garbage-collection ()
       (setq gc-cons-threshold doom-gc-cons-upper-limit))
     (defun doom|restore-garbage-collection ()
       ;; Defer it so that commands launched from the minibuffer can enjoy the
       ;; benefits.
       (run-at-time 1 nil (lambda () (setq gc-cons-threshold doom-gc-cons-threshold))))
     (add-hook 'minibuffer-setup-hook #'doom|defer-garbage-collection)
     (add-hook 'minibuffer-exit-hook  #'doom|restore-garbage-collection)
     ;; GC all sneaky breeky like
     (add-hook 'focus-out-hook #'garbage-collect))))


(if (ignore-errors (or after-init-time noninteractive))
    (setq gc-cons-threshold doom-gc-cons-threshold)
  ;; A big contributor to startup times is garbage collection. We up the gc
  ;; threshold to temporarily prevent it from running, then reset it later in
  ;; `doom|restore-startup-optimizations'.
  (setq gc-cons-threshold doom-gc-cons-upper-limit)
  ;; This is consulted on every `require', `load' and various path/io functions.
  ;; You get a minor speed up by nooping this.
  (setq file-name-handler-alist nil)
  ;; Not restoring these to their defaults will cause stuttering/freezes.
(add-hook 'after-init-hook #'doom|restore-startup-optimizations))

;; 显示加载时间
(defvar albert-init-time 'nil)
(defun albert-display-benchmark()
  (message "Emacs loaded %s packages in %.03fs"
           (length package-activated-list)
           (or albert-init-time
               (setq albert-init-time
                     (float-time (time-subtract (current-time) before-init-time))))))
(add-hook 'emacs-startup-hook #'albert-display-benchmark)
;; end of 启动优化

;; [2018-11-29 周四 22:28:22] 测试emacs启动需要30s+的问题
;; (toggle-debug-on-error)

;; [2018-11-30 周五 11:45:50] 这个函数是必须的，否则启动报错。
(package-initialize nil)

;; Override the packages with the git version of Org and other packages
;(add-to-list 'load-path "~/.emacs.d/lisp/org-8.2.7c/lisp")
;(add-to-list 'load-path "~/.emacs.d/lisp/org-8.2.7c/contrib/lisp")

;; Load the rest of the packages
(package-initialize t)
(setq package-enable-at-startup nil)
(require 'benchmark-init-modes)
(require 'benchmark-init)
(benchmark-init/activate)

;; (setq org-modules
(defvar org-modules
  '(;; org-w3m
    ;; org-bbdb
    ;; org-bibtex
    ;; org-docview
    ;; org-gnus
    ;; org-info
    ;; org-irc
    ;; org-mhe
    ;; org-rmail
    ))

(require 'org)
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "~/.emacs.d/Albert.org"))
;; (org-babel-load-file (expand-file-name "~/.emacs.d/Albert_org_config.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#181a26" "#FF0000" "#4eee94" "#FFFF00" "#0000cd" "#f08080" "#00FFFF" "#cccccc"])
 '(custom-safe-themes (quote (default)))
 '(fci-rule-color "#ffe4b5")
 '(jdee-db-active-breakpoint-face-colors (cons "#100e23" "#EE82EE"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#100e23" "#4eee94"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#100e23" "#7f7f7f"))
 '(objed-cursor-color "#FF0000")
 '(package-selected-packages
   (quote
    (neotree all-the-icons doom-modeline doom-themes py-autopep8 helm-ag helm-gtags ggtags go-mode xah-find window-numbering web-mode use-package spinner sesman seq queue powerline pkg-info paren-face org2blog markdown-mode magit highlight-parentheses helm-swoop evil-paredit elpy dired+ diminish benchmark-init)))
 '(safe-local-variable-values (quote ((encoding . UTF-8))))
 '(vc-annotate-background "#181a26")
 '(vc-annotate-color-map
   (list
    (cons 20 "#4eee94")
    (cons 40 "#89f362")
    (cons 60 "#c3f931")
    (cons 80 "#FFFF00")
    (cons 100 "#ffe100")
    (cons 120 "#ffc300")
    (cons 140 "#FFA500")
    (cons 160 "#fa982a")
    (cons 180 "#f58c55")
    (cons 200 "#f08080")
    (cons 220 "#f55555")
    (cons 240 "#fa2a2a")
    (cons 260 "#FF0000")
    (cons 280 "#df1f1f")
    (cons 300 "#bf3f3f")
    (cons 320 "#9f5f5f")
    (cons 340 "#ffe4b5")
    (cons 360 "#ffe4b5")))
 '(vc-annotate-very-old-color nil))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button))))))
