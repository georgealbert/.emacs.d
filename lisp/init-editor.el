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

  ;; Slow this down from 0.02 to prevent blocking in large or folded buffers
  ;; like magit while incrementally highlighting matches.
  ;; (setq-hook! 'magit-mode-hook evil-ex-hl-update-delay 0.2)
  ;; (setq-hook! 'so-long-minor-mode-hook evil-ex-hl-update-delay 0.25)

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
  :disabled t
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

(provide 'init-editor)
