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
	;; foo-bar vim认为就是一个word，emacs会认为是2个
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
  
  ;; 使用isearch时，增加search结束后的，高亮时间到30s
  (setq evil-flash-delay 30)
  ;; evil keymap
  (define-key evil-normal-state-map "go" 'goto-char)
  ;; (define-key evil-normal-state-map (kbd "C-]") 'counsel-etags-find-tag-at-point)
  ;; (define-key evil-visual-state-map (kbd "C-]") 'counsel-etags-find-tag-at-point)

  ;; ivy-occur
  ;; after counsel-ag, c-c c-o, press w to edit buffer, c-x c-s when finished, c-c c-k discard changes.
  (evil-set-initial-state 'ivy-occur-grep-mode 'normal)
  )

;; homepage: https://github.com/redguardtoo/evil-matchit
(use-package evil-matchit
  :hook (prog-mode . turn-on-evil-matchit-mode)
  )

;; doc: Comment/uncomment lines efficiently. Like Nerd Commenter in Vim
;; homepage: https://github.com/redguardtoo/evil-nerd-commenter
(use-package evil-nerd-commenter
  :defer t
  :config
  (evilnc-default-hotkeys t)
  )

(use-package ediff
  :ensure nil
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

;; doc: Colourful dired from seagle0128，比较轻量，dired-k的git用得太多了，有点慢
(use-package diredfl
  :defer t
  :hook (dired-initial-position . diredfl-mode)
        ;; (dired-after-readin . diredfl-mode)
  :config
  (defface my-diredfl-read-priv
  '((((background dark)) (:background "bg"))
    (t                   (:background "LightGray")))
    "*Face used for read privilege indicator (w) in Dired buffers."
    :group 'diredfl)
  (setq diredfl-read-priv 'my-diredfl-read-priv)

  (defface my-diredfl-write-priv
    '((((background dark)) (:background "bg"))
      (t                   (:background "Orchid")))
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
  ;; :hook (after-init . recentf-mode)
  :defer t
  :init (setq recentf-max-saved-items 50
              recentf-exclude
              '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                "^/tmp/" "^/var/folders/.+$" ; "^/ssh:"
                (lambda (file) (file-in-directory-p file package-user-dir))))
  :config (push (expand-file-name recentf-save-file) recentf-exclude))

(use-package awesome-pair
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/extensions/awesome-pair"
  :defer t
  :hook (prog-mode . awesome-pair-mode)
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
;; doc: Jump to things in Emacs tree-style
(use-package avy
  :defer t
  :bind (("C-:" . avy-goto-char)
         ("C-;" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  :config (setq avy-all-windows nil
                avy-all-windows-alt t
                avy-background t
                avy-style 'pre))

;; doc: a better *help* buffer
;; homepage: https://github.com/Wilfred/helpful
(use-package helpful
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

;; from redguardtoo, 快捷键 SPC ww
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
