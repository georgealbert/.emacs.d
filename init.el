;; This sets up the load path so that we can override it
(package-initialize nil)
;; Override the packages with the git version of Org and other packages
;(add-to-list 'load-path "~/.emacs.d/lisp/org-8.2.7c/lisp")
;(add-to-list 'load-path "~/.emacs.d/lisp/org-8.2.7c/contrib/lisp")
;; Load the rest of the packages
(package-initialize t)
(setq package-enable-at-startup nil)
(require 'org)
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "~/.emacs.d/Albert.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-time-mode t)
 '(package-selected-packages
   (quote
    (paren-face evil-paredit paredit window-numbering web-mode use-package powerline org2blog magit htmlize highlight-parentheses helm-swoop evil dired+ cider)))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-handled-backends (quote (Git))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button))))))
