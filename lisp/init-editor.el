;;; init-editor.el -*- lexical-binding: t; -*-

;; Don't autosave files or create lock/history/backup files. We don't want
;; copies of potentially sensitive material floating around, and we'll rely on
;; git and our own good fortune instead. Fingers crossed!
(setq auto-save-default nil ; Disable auto save
      create-lockfiles nil
      make-backup-files nil ; Forbide to make backup files
      ;; But have a place to store them in case we do use them...
      auto-save-list-file-name  "~/.emacs.d/backup/autosave"
      backup-directory-alist `(("." . "~/.emacs.d/backup/")))

;; F5插入当前时间  
;; e.g.
;; M-: (insert (format-time-string "[%Y-%m-%d %a %T]"))
(global-set-key [(f5)] '(lambda () (interactive) 
                          (insert (format-time-string "[%Y-%m-%d %a %T]"))))

(use-package evil
  :ensure t
  :defer 1
  :preface
  (setq evil-want-visual-char-semi-exclusive t
        evil-echo-state t
        evil-ex-search-vim-style-regexp t
        ;; more vim-like behavior
        ;; foo-bar 就是一个word，emacs会认为是2个
        evil-symbol-word-search t
        ;; cursor appearance
        ;; evil-normal-state-cursor 'box
        ;; evil-insert-state-cursor 'bar
        ;; 不习惯空心的光标
        ;; evil-visual-state-cursor 'hollow
        )

  :config 
  (evil-mode 1)
  ;; (eval-after-load 'helm-gtags
  ;;    '(progn
  ;;       (define-key evil-motion-state-map "C-]" 'helm-gtags-find-tag-from-here)))
  (eval-after-load 'ggtags
    '(progn
       (evil-make-overriding-map ggtags-mode-map 'normal)
       ;; force update evil keymaps after ggtags-mode loaded
       (add-hook 'ggtags-mode-hook #'evil-normalize-keymaps)))
  
  ;; Lazy load evil ex commands. From doom-emacs，好像看着还是load了evil-ex，不知道为什么
  ;; (delq! 'evil-ex features)
  ;; (add-transient-hook! 'evil-ex (provide 'evil-ex))

  ;; (setq evil-want-visual-char-semi-exclusive t)
  ;; (evil-select-search-module 'evil-search-module 'evil-search)
  ;;(setq isearch-hide-immediately nil)
  ;; (add-hook 'doom-escape-hook
  ;;   (defun +evil-disable-ex-highlights-h ()
  ;;     "Disable ex search buffer highlights."
  ;;     (when (evil-ex-hl-active-p 'evil-ex-search)
  ;;       (evil-ex-nohighlight)
  ;;       t)))

  ;; 使用isearch时，增加search结束后的，高亮时间到30s
  (setq evil-flash-delay 30)
  )

(use-package ediff
  :ensure nil
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-merge-split-window-function 'split-window-horizontally)
  (setq-default ediff-ignore-similar-regions t)
  (setq-default ediff-split-window-function 'split-window-horizontally)

  ;; refinement, 设置后按字符比较，refinement是改变的地方
  (setq-default ediff-forward-word-function 'forward-char)

  ;; 让diff能识别中文目录
  (setq process-coding-system-alist (cons '("diff" . (cp936 . cp936)) process-coding-system-alist)))

(use-package dired-k
  ;; :unless (featurep! +ranger)
  :defer t
  :hook (dired-initial-position . dired-k)
  :hook (dired-after-readin . dired-k-no-revert)
  :config
  (defun +dired*interrupt-process (orig-fn &rest args)
    "Fixes dired-k killing git processes too abruptly, leaving behind disruptive
     .git/index.lock files."
    (cl-letf (((symbol-function #'kill-process)
               (symbol-function #'interrupt-process)))
      (apply orig-fn args)))
  (advice-add #'dired-k--start-git-status :around #'+dired*interrupt-process)

  (defun +dired*dired-k-highlight (orig-fn &rest args)
    "Butt out if the requested directory is remote (i.e. through tramp)."
    (unless (file-remote-p default-directory)
      (apply orig-fn args)))
  (advice-add #'dired-k--highlight :around #'+dired*dired-k-highlight))

;; History
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package recentf
  :ensure nil
  ;; :hook (after-init . recentf-mode)
  :defer t
  :init (setq recentf-max-saved-items 100
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" ; "^/ssh:"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config (push (expand-file-name recentf-save-file) recentf-exclude))

(use-package awesome-pair
  :ensure nil
  ;; :disabled t
  :load-path "~/.emacs.d/site-lisp/extensions/awesome-pair"
  :defer t
  :hook (prog-mode . awesome-pair-mode)
  ;; :init
  ;; (dolist (hook (list
  ;;                'c-mode-common-hook
  ;;                'c-mode-hook
  ;;                'c++-mode-hook
  ;;                'java-mode-hook
  ;;                'haskell-mode-hook
  ;;                'emacs-lisp-mode-hook
  ;;                'lisp-interaction-mode-hook
  ;;                'lisp-mode-hook
  ;;                'maxima-mode-hook
  ;;                'ielm-mode-hook
  ;;                'sh-mode-hook
  ;;                'makefile-gmake-mode-hook
  ;;                'php-mode-hook
  ;;                'python-mode-hook
  ;;                'js-mode-hook
  ;;                'go-mode-hook
  ;;                'qml-mode-hook
  ;;                'jade-mode-hook
  ;;                'css-mode-hook
  ;;                'ruby-mode-hook
  ;;                'coffee-mode-hook
  ;;                'rust-mode-hook
  ;;                'qmake-mode-hook
  ;;                'lua-mode-hook
  ;;                'swift-mode-hook
  ;;                'web-mode-hook
  ;;                'markdown-mode-hook
  ;;                'llvm-mode-hook
  ;;                'conf-toml-mode-hook
  ;;                ))
  ;;   (add-hook hook '(lambda () (awesome-pair-mode 1))))
  :config
  (define-key awesome-pair-mode-map (kbd "(") 'awesome-pair-open-round)
  (define-key awesome-pair-mode-map (kbd "[") 'awesome-pair-open-bracket)
  (define-key awesome-pair-mode-map (kbd "{") 'awesome-pair-open-curly)
  (define-key awesome-pair-mode-map (kbd ")") 'awesome-pair-close-round)
  (define-key awesome-pair-mode-map (kbd "]") 'awesome-pair-close-bracket)
  (define-key awesome-pair-mode-map (kbd "}") 'awesome-pair-close-curly)
  (define-key awesome-pair-mode-map (kbd "%") 'awesome-pair-match-paren)
  (define-key awesome-pair-mode-map (kbd "\"") 'awesome-pair-double-quote)
  (define-key awesome-pair-mode-map (kbd "M-o") 'awesome-pair-backward-delete) 
  (define-key awesome-pair-mode-map (kbd "C-k") 'awesome-pair-kill)
  (define-key awesome-pair-mode-map (kbd "M-\"") 'awesome-pair-wrap-double-quote) 
  (define-key awesome-pair-mode-map (kbd "M-[") 'awesome-pair-wrap-bracket)
  (define-key awesome-pair-mode-map (kbd "M-{") 'awesome-pair-wrap-curly)
  (define-key awesome-pair-mode-map (kbd "M-(") 'awesome-pair-wrap-round)
  (define-key awesome-pair-mode-map (kbd "M-)") 'awesome-pair-unwrap)
  (define-key awesome-pair-mode-map (kbd "M-p") 'awesome-pair-jump-right) 
  (define-key awesome-pair-mode-map (kbd "M-n") 'awesome-pair-jump-left) 
  (define-key awesome-pair-mode-map (kbd "M-:") 'awesome-pair-jump-out-pair-and-newline)
  )

;; from seagle0128/.emacs.d/lisp/init-edit.el
;; Jump to things in Emacs tree-style
(use-package avy
  :defer t
  :bind (("C-:" . avy-goto-char)
         ("C-;" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  ;; :hook (after-init . avy-setup-default)
  :config (setq avy-all-windows nil
                avy-all-windows-alt t
                avy-background t
                avy-style 'pre))

;; https://github.com/Wilfred/helpful
(use-package helpful
  ;; a better *help* buffer
  :defer t
  :commands helpful--read-symbol
  :init
  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key)
  ;; (global-set-key [remap describe-symbol]   #'doom/describe-symbol)

  ;; (defun doom-use-helpful-a (orig-fn &rest args)
  ;;   "Force ORIG-FN to use helpful instead of the old describe-* commands."
  ;;   (cl-letf (((symbol-function #'describe-function) #'helpful-function)
  ;;             ((symbol-function #'describe-variable) #'helpful-variable))
  ;;     (apply orig-fn args)))

  ;; (after! apropos
  ;;   ;; patch apropos buttons to call helpful instead of help
  ;;   (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
  ;;     (button-type-put
  ;;      fun-bt 'action
  ;;      (lambda (button)
  ;;        (helpful-callable (button-get button 'apropos-symbol)))))
  ;;   (dolist (var-bt '(apropos-variable apropos-user-option))
  ;;     (button-type-put
  ;;      var-bt 'action
  ;;      (lambda (button)
  ;;        (helpful-variable (button-get button 'apropos-symbol))))))
  )


(provide 'init-editor)
