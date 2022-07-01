;;; init-bridge.el -*- lexical-binding: t; -*-

(use-package lsp-bridge
  :ensure nil
  :defer 2
  :bind (:map lsp-bridge-mode
              ("C-s-j" . lsp-bridge-jump-to-next-diagnostic) ;显示下一个错误
              ("C-s-k" . lsp-bridge-jump-to-prev-diagnostic) ;显示上一个错误
              ("C-s-n" . lsp-bridge-popup-documentation-scroll-up) ;向下滚动文档
              ("C-s-p" . lsp-bridge-popup-documentation-scroll-down) ;向上滚动文档
              )
  :load-path "~/.emacs.d/site-lisp/extensions/lsp-bridge"
  ;; :hook ((python-mode sh-mode lua-mode) . (lambda ()
  ;;                                           ;; (require 'lsp-bridge-icon) ;; show icon for completion items, optional
  ;;                                           ;; (require 'lsp-bridge-orderless) ;; make lsp-bridge support fuzzy match, optional
  ;;                                           ;; (setq-local corfu-auto nil)
  ;;                                           (lsp-bridge-mode 1)
  ;;                                           ))
  :config
  (setq lsp-bridge-default-mode-hooks
        (remove 'org-mode-hook lsp-bridge-default-mode-hooks))

  (setq lsp-bridge-python-command "/Users/albert/.virtualenvs/pandas/bin/python")

  ;; (setq lsp-bridge-completion-provider 'corfu)

  ;; (require 'corfu)
  ;; (require 'corfu-info)
  ;; (require 'corfu-history)
  ;; (global-corfu-mode)               ;; use corfu as completion ui
  ;; (corfu-history-mode t)

  (global-lsp-bridge-mode)

  ;; (setq lsp-bridge-enable-log t)
  )

(provide 'init-lsp-bridge)
