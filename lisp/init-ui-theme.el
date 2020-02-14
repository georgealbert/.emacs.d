;;; init-ui-theme.el -*- lexical-binding: t; -*-

(use-package doom-themes
  :config
  ;; 我的doom-deeper-blue-theme.el在 ~/.emacs.d/my_elisp 目录中
  (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/my_elisp"))

  (setq ;; 禁用粗体，否则org-mode的outline字体太难看
        doom-themes-enable-bold nil
        doom-themes-enable-italic t)

  ;; 在load-theme之前设置，让modeline更亮一点，
  ;; (setq doom-deeper-blue-brighter-modeline nil)

  ;; 我自己的theme
  (load-theme 'doom-deeper-blue t)
  
  ;; org-mode的序号带颜色显示
  (doom-themes-org-config))
  
;; doom-modeline要在 package winum 之前初始化，不然winum的frame编号显示有问题.
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (unless after-init-time
    ;; prevent flash of unstyled modeline at startup
    (setq-default mode-line-format nil))

  (setq doom-modeline-bar-width 3)

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
  
  ;; (size-indication-mode +1) ; filesize in modeline
  ;; modeline中的时间格式设置 [2014-11-21 周五 10:35:59]

  ;; (setq display-time-24hr-format t)
  ;; (setq display-time-use-mail-icon t)
  ;; (setq display-time-interval 60)

  (setq display-time-day-and-date t)
  (setq display-time-format "%Y-%m-%d %a %H:%M")
  (setq display-time-default-load-average nil)

  (if (display-graphic-p)
      (display-time)))

(provide 'init-ui-theme)
