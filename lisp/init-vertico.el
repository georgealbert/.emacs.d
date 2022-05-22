;;; init-vertico.el -*- lexical-binding: t; -*-

(use-package vertico
  :defer 0.5
  :bind
  (:map vertico-map ("DEL" . vertico-directory-delete-char))
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t
        completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

  (vertico-mode t)
  )

(use-package marginalia
  :after vertico
  :config
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
  (marginalia-mode t))

(use-package all-the-icons-completion
  :defer t)

(use-package consult
  :defer t
  :bind
  (
   ;; ("C-s" . consult-line)
   ("s-f". consult-ripgrep)
   ("C-c n" . consult-buffer))
  :config
  ;; `consult-recent-file' needs to have `recentf-mode' on to work correctly.
  (recentf-mode +1)

  (setq ;; consult-project-root-function #'doom-project-root
        consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)
  )

(use-package embark
  :defer t
  :bind
  (("C-;" . embark-act)); to be moved to :config default if accepted
  :init
  (setq which-key-use-C-h-commands nil
        prefix-help-command #'embark-prefix-help-command)
)

;; (use-package corfu
;;   :defer 0.5
;;   :init
;;   (global-corfu-mode)
;;   :config
;;   (setq corfu-auto t))

;; (use-package orderless
;;   :after corfu
;;   :init
;;   (setq completion-styles '(basic partial-completion orderless)
;;   ))

(use-package embark-consult
  :after (embark consult)
  :config
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(provide 'init-vertico)
