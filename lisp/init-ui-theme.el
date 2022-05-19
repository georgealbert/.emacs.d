;;; init-ui-theme.el -*- lexical-binding: t; no-byte-compile: t; -*-


(use-package emacs
  :init
  :disabled t
  ;; Add all your customizations prior to loading the themes
  (setq ;; modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        ;; modus-themes-region '(bg-only no-extend)
        ;; modus-themes-hl-line '(underline accented)
        modus-themes-hl-line '(underline intense)
        )
  :config
  ;; Load the theme of your choice:
  (load-theme 'deeper-blue)
  ;; (load-theme 'modus-vivendi)
  )

(use-package doom-themes
  :config
  ;; :disabled t
  (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/theme"))

  (setq ;; 禁用粗体，否则org-mode的outline字体太难看
        doom-themes-enable-bold nil
        doom-themes-enable-italic t)

  ;; 在load-theme之前设置，让modeline更亮一点，
  ;; (setq doom-deeper-blue-brighter-modeline nil)

  (load-theme 'doom-deeper-blue t)
  ;; (load-theme 'doom-one t)
  
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
  
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
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
  
  ;; evil state icon
  (setq doom-modeline-modal-icon nil)

  ;; [2020-01-05 周日 21:56:42] 从find-file-hook看见有hook，去掉
  (setq doom-modeline-persp-name nil)

  (setq doom-modeline-buffer-file-name-style 'file-name)
  :config
  (setq display-time-day-and-date t)
  ;; (setq display-time-format "%Y-%m-%d %a %H:%M")
  (setq display-time-format "%-m月%-d日 %a %H:%M")
  (setq display-time-default-load-average nil)
  
  ;; 在modeline显示时间
  ;; (display-time)
  
  ;; 列号是从0开始的。
  (column-number-mode +1)
  
  ;; 在modeline显示buffer或文件的大小
  ;; (size-indication-mode +1) 

  ;; (doom-modeline-mode +1)
  )

(provide 'init-ui-theme)
