;;; init-bridge.el -*- lexical-binding: t; -*-

(use-package lsp-bridge
  :ensure nil
  :defer t
  :load-path "~/.emacs.d/site-lisp/extensions/lsp-bridge"
  :hook ((python-mode sh-mode lua-mode) . (lambda ()
                                            (require 'lsp-bridge-orderless) ;; make lsp-bridge support fuzzy match, optional
                                            (require 'lsp-bridge-icon) ;; show icon for completion items, optional
                                            (setq-local corfu-auto nil)
                                            (lsp-bridge-mode 1)))
  :config
  (setq lsp-bridge-python-command "/Users/albert/.virtualenvs/pandas/bin/python")

  (setq lsp-bridge-completion-provider 'corfu)

  ;; (require 'corfu)
  ;; (require 'corfu-info)
  ;; (require 'corfu-history)
  ;; (global-corfu-mode)               ;; use corfu as completion ui
  ;; (corfu-history-mode t)

  ;; (global-lsp-bridge-mode)

  ;; (setq lsp-bridge-enable-log t)
  )

(provide 'init-lsp-bridge)
