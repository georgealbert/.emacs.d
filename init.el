;;; init.el -*- lexical-binding: t; -*-

(defvar albert-dumped nil
  "non-nil when a dump file is loaded (because dump.el sets this variable).")

(defmacro albert-if-dump (then &rest else)
  "Evaluate IF if running with a dump file, else evaluate ELSE."
  (declare (indent 1))
  `(if albert-dumped
       ,then
     ,@else))

(albert-if-dump
    (progn
      (message "using pdump...")
      (setq load-path albert-dumped-load-path)
      (global-undo-tree-mode -1)
      (global-font-lock-mode)
      (add-hook 'after-init-hook
                (lambda ()
                  (save-excursion
                    (switch-to-buffer "*scratch*")
                    (fundamental-mode)))))
  ;; add load-path’s and load autoload files
  (package-initialize))

;; load theme
(when window-system
  (albert-if-dump
      (enable-theme 'doom-deeper-blue)))

;; [2018-11-30 周五 11:45:50] 这个函数是必须的，否则启动报错。
;; (package-initialize nil)

;; [2019-10-30 Wed 17:34:45] 27.0 does not need this
(setq package-enable-at-startup nil)

(require 'benchmark-init-modes)
(require 'benchmark-init)
(benchmark-init/activate)

;; (require 'esup)
;; (autoload 'esup "esup" "Emacs Start Up Profiler." nil)

(defvar org-modules
  '())

(require 'ob-tangle)
(org-babel-load-file (expand-file-name "~/.emacs.d/Albert.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/Albert_org_config.org"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes '(default))
 '(package-selected-packages
   '(ace-window gcmh treemacs treemacs-evil treemacs-projectile treemacs-icons-dired lsp-python-ms ccls flycheck-posframe dired-k helm-ag doom-modeline doom-themes py-autopep8 flycheck pyim ggtags go-mode web-mode use-package spinner sesman seq queue pkg-info paren-face org2blog markdown-mode magit highlight-parentheses helm-swoop evil-paredit benchmark-init)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-doc-background ((t (:background nil))))
 '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button))))))
