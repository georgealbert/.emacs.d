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
   ("s-r". albert-consult-rg-project-root)
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

  (defun albert-consult--dominating-file (file &optional dir)
    "Look up directory hierarchy for FILE, starting in DIR.
Like `locate-dominating-file', but DIR defaults to
`default-directory' and the return value is expanded."
    (and (setq dir (locate-dominating-file (or dir default-directory) file))
         (expand-file-name dir)))

  (defun albert-consult--git-root ()
    "Return root of current project or nil on failure.
Use the presence of a \".git\" file to determine the root."
    (albert-consult--dominating-file ".git"))

  (defun albert-consult-rg-project-root (&optional query)
    "consult-rg-project-root, QUERY."
    (interactive)
    (let ((rootdir (albert-consult--git-root)))
      (unless rootdir
        (error "Could not find the project root.  Create a git, hg, or svn repository there first"))
      (consult-ripgrep rootdir)))
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
