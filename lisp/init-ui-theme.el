;;; init-ui-theme.el -*- lexical-binding: t; -*-

;; (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/my_elisp"))
;; (load-theme 'deeper-blue t)

(use-package doom-themes
  ;; :demand t
  :config
  ;; 我的doom-deeper-blue-theme.el在 ~/.emacs.d/my_elisp 目录中
  (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/my_elisp"))

  (setq ;; 禁用粗体，否则org-mode的outline字体太难看
        doom-themes-enable-bold nil
        doom-themes-enable-italic t)

  ;; 在load-theme之前设置，让modeline更亮一点，
  ;; (setq doom-deeper-blue-brighter-modeline nil)

  (load-theme 'doom-deeper-blue t)
  
  ;; org-mode的序号带颜色显示
  (doom-themes-org-config)

  ;; UGLY HACK!
  ;; Since Emacs 27 change the default behaviour of face.
  ;; We use this to keep backward compatibility.
  ;; Waiting upstream to fix this.
  ;; (when (>= emacs-major-version 27)
  ;;   (dolist (f (face-list))
  ;;     (set-face-attribute f nil :extend t)))
  )
  
;; doom-modeline要在 package winum 之前初始化，不然winum的frame编号显示有问题.
(use-package doom-modeline
  ;; 使用native-comp的undo-tree和doom-modeline时，在undo-tree必须在doom-modeline之前启动，否则advice-add undo-tree-undo-1时有问题
  ;; :after undo-tree
  :init
  (unless after-init-time
    ;; prevent flash of unstyled modeline at startup
    (setq-default mode-line-format nil))

  (setq doom-modeline-bar-width 3)

  ;; Whether show `all-the-icons' or not (if nil nothing will be showed).
  (setq doom-modeline-icon nil)
  
  ;; Whether show the icon for major mode. It respects `doom-modeline-icon'.
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-minor-modes nil)
  
  (setq doom-modeline-lsp nil)
  
  (setq doom-modeline-mu4e nil)
  (setq doom-modeline-github nil)
  (setq doom-modeline-persp-name nil)
  (setq doom-modeline-irc nil)
  
  ;; 2019.11.06修改为图标的了，不好看
  (setq doom-modeline-evil-state-icon nil)
  
  ;; 2019.11.22又改成下面这个变量了
  (setq doom-modeline-modal-icon nil)

  ;; [2020-01-05 周日 21:56:42] 从find-file-hook看见有hook，去掉
  (setq doom-modeline-persp-name nil)
  :config

  (setq display-time-day-and-date t)
  (setq display-time-format "%Y-%m-%d %a %H:%M")
  (setq display-time-default-load-average nil)
  
  ;; 在modeline显示时间
  (display-time)
  
  ;; 列号是从0开始的。
  (column-number-mode +1)
  
  ;; 在modeline显示buffer或文件的大小
  ;; (size-indication-mode +1) 

  (doom-modeline-mode +1)
  )

(provide 'init-ui-theme)
