;;; init-bridge.el -*- lexical-binding: t; -*-

(use-package lsp-bridge
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/extensions/lsp-bridge"
  :hook ((python-mode sh-mode lua-mode) . (lambda ()
                         (require 'lsp-bridge-orderless) ;; make lsp-bridge support fuzzy match, optional
                         (require 'lsp-bridge-icon) ;; show icon for completion items, optional
                         (setq-local corfu-auto nil)
                         (lsp-bridge-mode 1)
                         ))
  :config
  (setq lsp-bridge-python-command "/Users/albert/.virtualenvs/pandas/bin/python")

  ;; (setq lsp-bridge-enable-log t)
  ;; (dolist (hook (list
  ;;                'python-mode-hook
  ;;                ))
  ;;   (add-hook hook (lambda ()
  ;;                    (setq-local corfu-auto nil)  ;; let lsp-bridge control when popup completion frame
  ;;                    (lsp-bridge-mode 1)
  ;;                    )))
  )

(provide 'init-lsp-bridge)