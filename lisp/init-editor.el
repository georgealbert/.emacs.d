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
(global-set-key [(f5)] #'(lambda () (interactive) 
                          (insert (format-time-string "[%Y-%m-%d %a %T]"))))

(use-package evil
  :ensure t
  :defer 1
  :preface
  (setq evil-want-visual-char-semi-exclusive t)
  (setq evil-echo-state t
        evil-ex-search-vim-style-regexp t
        ;; foo-bar vim认为就是一个word，emacs会认为是2个
        evil-symbol-word-search t

        ;; emacs 29.0.50, org mode TAB
        ;; org-cycle broken while using evil https://github.com/emacs-evil/evil/issues/1505
        evil-want-C-i-jump nil

        evil-undo-system (cond (EMACS28+ 'undo-redo))
        )
  :config 
  (evil-mode 1)

  (eval-after-load 'ggtags
    '(progn
       (evil-make-overriding-map ggtags-mode-map 'normal)
       ;; force update evil keymaps after ggtags-mode loaded
       (add-hook 'ggtags-mode-hook #'evil-normalize-keymaps)))
  
  ;; 使用isearch时，增加search结束后的，高亮时间到30s
  (setq evil-flash-delay 30)

  ;; evil re-assign "M-." to `evil-repeat-pop-next` which I don't use actually.
  ;; Restore "M-." to original binding command
  (define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)

  (define-key evil-normal-state-map "u" 'vundo)

  ;; evil keymap
  (define-key evil-normal-state-map "go" 'goto-char)
  ;; (define-key evil-normal-state-map (kbd "C-]") 'counsel-etags-find-tag-at-point)
  ;; (define-key evil-visual-state-map (kbd "C-]") 'counsel-etags-find-tag-at-point)

  ;; https://github.com/redguardtoo/emacs.d/lisp/init.evil.el
  (define-key evil-normal-state-map "gh" 'beginning-of-defun)

  ;; I prefer Emacs way after pressing ":" in evil-mode
  (define-key evil-ex-completion-map (kbd "C-a") 'move-beginning-of-line)
  (define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
  (define-key evil-ex-completion-map (kbd "M-p") 'previous-complete-history-element)
  (define-key evil-ex-completion-map (kbd "M-n") 'next-complete-history-element)

  (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-insert-state-map (kbd "C-k") 'kill-line)

  (evil-define-key 'motion 'lsp-bridge-mode (kbd "C-]") 'lsp-bridge-find-def)
  (evil-define-key 'motion 'lsp-bridge-mode (kbd "gd") 'lsp-bridge-find-def)
  (evil-define-key 'normal 'lsp-bridge-mode (kbd "C-t") 'lsp-bridge-find-def-return)
  (evil-define-key 'normal 'lsp-bridge-mode (kbd "gr") 'lsp-bridge-find-references)
  (evil-define-key 'normal 'lsp-bridge-mode (kbd "gi") 'lsp-bridge-find-impl)
  (evil-define-key 'normal 'lsp-bridge-mode (kbd "K") 'lsp-bridge-popup-documentation)

  (evil-define-key 'insert 'corfu-mode (kbd "C-n") 'corfu-next)
  (evil-define-key 'insert 'corfu-mode (kbd "C-p") 'corfu-previous)

  (evil-define-key 'insert 'acm-mode (kbd "C-n") 'acm-select-next)
  (evil-define-key 'insert 'acm-mode (kbd "C-p") 'acm-select-prev)

  ;; github.com/redguardtoo/.emacs.d/lisp/init-evil.el
  ;; specify major mode uses Evil (vim) NORMAL state or EMACS original state.
  ;; You may delete this setup to use Evil NORMAL state always.
  (dolist (p '((minibuffer-inactive-mode . emacs)
               (calendar-mode . emacs)
               (special-mode . emacs)
               ;; (grep-mode . emacs)
               (Info-mode . emacs)
               ;; (term-mode . emacs)
               (sdcv-mode . emacs)
               ;; (anaconda-nav-mode . emacs)
               (log-edit-mode . emacs)
               (vc-log-edit-mode . emacs)
               ;; (magit-log-edit-mode . emacs)
               (w3m-mode . emacs)
               (gud-mode . emacs)
               (help-mode . emacs)
               ;; (eshell-mode . emacs)
               ;; (shell-mode . emacs)
               (xref--xref-buffer-mode . emacs)
               ;;(message-mode . emacs)
               ;; (epa-key-list-mode . emacs)
               ;; (fundamental-mode . emacs)
               (woman-mode . emacs)
               ;; (sr-mode . emacs)
               (profiler-report-mode . emacs)
               ;; (dired-mode . emacs)
               (compilation-mode . emacs)
               ;; (speedbar-mode . emacs)
               ;; (ivy-occur-mode . emacs)
               ;; (ffip-file-mode . emacs)

               (lsp-bridge-ref-mode . emacs)
               (color-rg-mode . emacs)
               ;;
               ;; ivy-occur-grep-mode
               ;;
               ;; after counsel-rg, c-c c-o, press w to edit buffer, c-x c-s when finished, c-c c-k discard changes.
               (ivy-occur-grep-mode . normal)

               (messages-buffer-mode . normal)
               (js2-error-buffer-mode . emacs)))
    (evil-set-initial-state (car p) (cdr p)))
  )

;; homepage: https://github.com/redguardtoo/evil-matchit
(use-package evil-matchit
  :defer t
  ;; :hook (prog-mode . turn-on-evil-matchit-mode)
  :hook (doom-first-input . turn-on-evil-matchit-mode)
  )

;; doc: Comment/uncomment lines efficiently. Like Nerd Commenter in Vim
;; homepage: https://github.com/redguardtoo/evil-nerd-commenter
;; shortcut: M-; / ,cc / ,cl
(use-package evil-nerd-commenter
  :defer t
  :config
  (evilnc-default-hotkeys t)
  )

(use-package ediff
  :ensure nil
  :defer t
  :hook(;; show org ediffs unfolded
        ;; (ediff-prepare-buffer . outline-show-all)
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
  (when IS-WINDOWS
    (setq process-coding-system-alist (cons '("diff" . (cp936 . cp936)) process-coding-system-alist)))

  ;; https://oremacs.com/2015/01/17/setting-up-ediff/
  ;; https://github.com/abo-abo/oremacs/blob/github/modes/ora-ediff.el
  (defun ora-ediff-jk ()
    (define-key ediff-mode-map "j" 'ediff-next-difference)
    (define-key ediff-mode-map "k" 'ediff-previous-difference))

  (add-hook 'ediff-keymap-setup-hook #'ora-ediff-jk)

  (defun ora-ediff-prepare-buffer ()
    (when (memq major-mode '(org-mode emacs-lisp-mode))
      (outline-show-all))
    ;; (when (> (max-line-width) 150)
    ;;   (visual-line-mode))
    )

  (add-hook 'ediff-prepare-buffer-hook 'ora-ediff-prepare-buffer)
  )

;; doc: Colourful dired from seagle0128/.emacs.d/lisp/init-dired.el，比较轻量，dired-k的git用得太多了，有点慢
(use-package diredfl
  :defer t
  :hook (dired-mode . diredfl-mode)
  :config
  (defface my-diredfl-read-priv
  '((((background dark)) (:background "#181a26"))
    (t                   (:background "LightGray")))
    "*Face used for read privilege indicator (r) in Dired buffers."
    :group 'diredfl)
  (setq diredfl-read-priv 'my-diredfl-read-priv)

  (defface my-diredfl-write-priv
    '((((background dark)) (:background "#181a26"))
      (t                   (:background "orchid")))
    "*Face used for write privilege indicator (w) in Dired buffers."
    :group 'diredfl)
  (setq diredfl-write-priv 'my-diredfl-write-priv)
  )

;; History
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package recentf
  :ensure nil
  :defer t
  :init (setq recentf-max-saved-items 50
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" ; "^/ssh:"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config (push (expand-file-name recentf-save-file) recentf-exclude))

;; from seagle0128/.emacs.d/lisp/init-edit.el
;; doc: Jump to things in Emacs tree-style
(use-package avy
  :defer t
  :bind (("C-;" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  :config (setq avy-all-windows nil
                avy-all-windows-alt t
                avy-background t
                avy-style 'pre))

;; homepage: https://github.com/Wilfred/helpful
;; doc: a better *help* buffer
(use-package helpful
  :defer t
  :commands helpful--read-symbol
  :init
  ;; https://github.com/hlissner/doom-emacs/commit/c6d3ceef7e8f3abdfce3cd3e51f1e570603bd230
  ;; fix: void-variable read-symbol-positions-list w/ helpful
  (when EMACS29+
    ;; REVIEW See Wilfred/elisp-refs#35. Remove once fixed upstream.
    (defvar read-symbol-positions-list nil))

  (global-set-key [remap describe-function] #'helpful-callable)
  (global-set-key [remap describe-command]  #'helpful-command)
  (global-set-key [remap describe-variable] #'helpful-variable)
  (global-set-key [remap describe-key]      #'helpful-key)
  (global-set-key [remap describe-symbol]   #'helpful-symbol))

;; from github.com/redguardtoo/.emacs.d/lisp/init-essential.el, 快捷键 `SPC ww'
;; {{ narrow region
(defun narrow-to-region-indirect-buffer-maybe (start end use-indirect-buffer)
  "Indirect buffer could multiple widen on same file."
  (if (region-active-p) (deactivate-mark))
  (if use-indirect-buffer
      (with-current-buffer (clone-indirect-buffer
                            (generate-new-buffer-name
                             (format "%s-indirect-:%s-:%s"
                                     (buffer-name)
                                     (line-number-at-pos start)
                                     (line-number-at-pos end)))
                            'display)
        (narrow-to-region start end)
        (goto-char (point-min)))
      (narrow-to-region start end)))

;; @see https://gist.github.com/mwfogleman/95cc60c87a9323876c6c
;; fixed to behave correctly in org-src buffers; taken from:
;; https://lists.gnu.org/archive/html/emacs-orgmode/2019-09/msg00094.html
(defun narrow-or-widen-dwim (&optional use-indirect-buffer)
  "If the buffer is narrowed, it widens.
 Otherwise, it narrows to region, or Org subtree.
If USE-INDIRECT-BUFFER is not nil, use `indirect-buffer' to hold the widen content."
  (interactive "P")
  (cond
   ((and (not use-indirect-buffer) (buffer-narrowed-p))
    (widen))

   ((and (not use-indirect-buffer)
         (eq major-mode 'org-mode)
         (fboundp 'org-src-edit-buffer-p)
         (org-src-edit-buffer-p))
    (org-edit-src-exit))

   ;; narrow to region
   ((region-active-p)
    (narrow-to-region-indirect-buffer-maybe (region-beginning)
                                            (region-end)
                                            use-indirect-buffer))

   ;; narrow to specific org element
   ((derived-mode-p 'org-mode)
    (cond
     ((ignore-errors (org-edit-src-code)) t)
     ((ignore-errors (org-narrow-to-block) t))
     ((ignore-errors (org-narrow-to-element) t))
     (t (org-narrow-to-subtree))))

   ((derived-mode-p 'diff-mode)
    (let* (b e)
      (save-excursion
        ;; If the (point) is already beginning or end of file diff,
        ;; the `diff-beginning-of-file' and `diff-end-of-file' return nil
        (setq b (progn (diff-beginning-of-file) (point)))
        (setq e (progn (diff-end-of-file) (point))))
      (when (and b e (< b e))
        (narrow-to-region-indirect-buffer-maybe b e use-indirect-buffer))))

   ((derived-mode-p 'prog-mode)
    (mark-defun)
    (narrow-to-region-indirect-buffer-maybe (region-beginning)
                                            (region-end)
                                            use-indirect-buffer))
   (t (error "Please select a region to narrow to"))))
;; }}

(provide 'init-editor)
