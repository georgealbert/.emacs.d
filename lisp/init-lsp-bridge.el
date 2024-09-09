;;; init-bridge.el -*- lexical-binding: t; -*-

(use-package lsp-bridge
  :ensure nil
  ;; :defer 5
  ;; :hook ( (python-mode rust-mode bash-mode sh-mode) . lsp-bridge-mode)
  :hook ( (python-mode java-mode) . lsp-bridge-mode)
  :bind (:map lsp-bridge-mode
              ("C-s-j" . lsp-bridge-diagnostic-jump-next) ;显示下一个错误
              ("C-s-k" . lsp-bridge-diagnostic-jump-prev) ;显示上一个错误
              ("C-s-n" . lsp-bridge-popup-documentation-scroll-up) ;向下滚动文档
              ("C-s-p" . lsp-bridge-popup-documentation-scroll-down) ;向上滚动文档
              )
  :load-path "~/.emacs.d/site-lisp/extensions/lsp-bridge"
  :init
  ;; for java
  ;; https://github.com/manateelazycat/lsp-bridge/wiki/Eclipse-JDT-Language-Server
  (require 'lsp-bridge-jdtls) ;; 根据项目自动生成自定义配置，添加必要的启动参数
  :config
  (setq lsp-bridge-default-mode-hooks
        (remove 'org-mode-hook lsp-bridge-default-mode-hooks))

  (setq lsp-bridge-default-mode-hooks
        (remove 'emacs-lisp-mode-hook lsp-bridge-default-mode-hooks))


  (setq lsp-bridge-default-mode-hooks
        (remove 'bash-mode-hook lsp-bridge-default-mode-hooks))

  (setq lsp-bridge-flash-region-delay 10.0)
  ;; (setq lsp-bridge-python-command "/Users/albert/.virtualenvs/pandas/bin/python")
  ;; (setq lsp-bridge-python-command "/usr/local/var/pyenv/shims/python")

  (global-lsp-bridge-mode)

  ;; tooltip的字体大小从默认的130增加到160
  (setq lsp-bridge-lookup-doc-tooltip-font-height 160)

  ;; 关闭tempel可以让补全更快些
  ;; https://emacs-china.org/t/lsp-bridge/20786/2341
  (setq acm-enable-tempel nil)

  ;; (setq lsp-bridge-enable-auto-import t) ;; 开启自动导入依赖，目前没有code action。补全时可以通过这个导入相应的依赖，建议开启。
  ;; (setq lsp-bridge-jdtls-jvm-args '("-javaagent:/Users/albert/.local/share/nvim/mason/packages/jdtls/lombok.jar"))

  ;; (setq lsp-bridge-enable-log t)
  )

(provide 'init-lsp-bridge)
