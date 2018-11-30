;; This sets up the load path so that we can override it

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
(require 'benchmark-init)
(require 'org)
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "~/.emacs.d/Albert.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/Albert_org_config.org"))
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(column-number-mode t)
;;  '(custom-safe-themes
;;    (quote
;;     ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
;;  '(display-time-mode t)
;;  '(package-selected-packages
;;    (quote
;;     (benchmark-init elpy ox-publish xah-find color-theme-solarized paren-face evil-paredit paredit window-numbering web-mode use-package powerline org2blog magit htmlize highlight-parentheses helm-swoop evil dired+)))
;;  '(show-paren-mode t)
;;  '(tool-bar-mode nil)
;; ;; '(vc-handled-backends (quote (Git)))
;;  )
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button))))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (xah-find window-numbering web-mode use-package spinner sesman seq queue powerline pkg-info paren-face org2blog markdown-mode magit highlight-parentheses helm-swoop evil-paredit elpy dired+ diminish color-theme-solarized benchmark-init))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button))))))
