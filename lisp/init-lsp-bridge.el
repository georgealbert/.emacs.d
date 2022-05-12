;;; init-bridge.el -*- lexical-binding: t; -*-

(use-package corfu
  :ensure t
  :defer t
  :config
  (global-corfu-mode)
)

(use-package orderless
  :ensure t
  :defer t)

(use-package lsp-bridge
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/extensions/lsp-bridge"
  :hook (python-mode . (lambda ()
                         (require 'lsp-bridge-orderless) ;; make lsp-bridge support fuzzy match, optional
                         (require 'lsp-bridge-icon) ;; show icon for completion items, optional
                         (setq-local corfu-auto nil)
                         (lsp-bridge-mode 1)
                         ))
  ;; :bind (:map lsp-bridge-mode-map
  ;; :bind (:map python-mode-map
  ;;             ;; ([remap xref-find-definitions] . lsp-bridge-find-define)
  ;;             ([remap xref-find-definitions] . lsp-bridge-find-define)
  ;;             ;; ([remap evil-goto-definitions] . lsp-bridge-jump)
  ;;             ;; ("C-c d" . lsp-bridge-jump)
  ;;             ("C-c d" . lsp-bridge-find-define)
  ;;             )
  ;; :bind (("s-8" . lsp-bridge-find-define)
  ;;        ("s-9" . lsp-bridge-return-from-def))
  ;; :init
  ;; (setq lsp-bridge-mode-map
  ;;       (let ((map (make-sparse-keymap)))
  ;;         (define-key map (kbd "s-8") 'lsp-bridge-find-define)
  ;;         (define-key map (kbd "s-9") 'lsp-bridge-return-from-def)
  ;;         map))
  ;; (general-define-key
  ;;  :states 'motion
  ;;  :keymaps 'lsp-bridge-mode-map
  ;;  "g d" 'lsp-bridge-find-define
  ;;  "c-]" 'lsp-bridge-find-define
  ;;  "c-t" 'lsp-bridge-return-from-def)
  ;; (general-define-key
  ;;  :states 'normal
  ;;  :keymaps 'lsp-bridge-mode-map
  ;;  "g d" 'lsp-bridge-find-define
  ;;  ;; "c-]" 'lsp-bridge-find-define
  ;;  "g b" 'lsp-bridge-return-from-def)
  :config
  (setq lsp-bridge-python-command "/Users/albert/.virtualenvs/pandas/bin/python")

  ;; (dolist (hook (list
  ;;                'python-mode-hook
  ;;                ))
  ;;   (add-hook hook (lambda ()
  ;;                    (setq-local corfu-auto nil)  ;; let lsp-bridge control when popup completion frame
  ;;                    (lsp-bridge-mode 1)
  ;;                    )))
  )

(provide 'init-lsp-bridge)
