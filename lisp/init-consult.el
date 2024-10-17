;;; init-consult.el --- Initialize completion configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Modern completion configuration.
;;

;; Pros:
;; 4. 要拼音首字母查询不需要像ivy里一样，直接输入即可，也不用写个函数

;; TODO:
;; 1. 如何实现counsel-buffer-or-recentf - 搞不定
;; 2. 显示对齐 - 已解决
;; 3. posframe不显示
;; 5. consult-buffer里面的文件名的颜色不好看 - 已解决
;; 6. 在dired中不能用consult-rg-current-dir - 已解决，唉，快捷键配置错了，应该是 s-f，写错 S-f 了。
;; 7. consult-line用的orderless match颜色不好看 - 已解决
;; 8. consult-line查询后，在normal mode中按n不能像ivy一样查询 - 已解决，按 up和down键就可以了，不用像在ivy里面先按 C-o，然后按 j和k 键了。
;; 9. consult-recent-file中，不能像counsel-buffer-or-recenf一样显示哪些文件是打开的，可能要参考counsel写一个，有点难啊
;; 10. M-x不能显示执行历史 - 已解决，在amx里新增了consult做为后端。直接用consult的问题是没有历史记录，只能打开emacs后执行过的。也没有找到怎么控制consult在执行extended-excute-command时不进行sort

;;; Code:

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Only list the commands of the current modes
  (when (boundp 'read-extended-command-predicate)
    (setq read-extended-command-predicate
          #'command-completion-default-include-p))

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

;; Support Pinyin
(use-package pinyinlib
  :after orderless
  :load-path "~/.emacs.d/site-lisp/extensions/pinyinlib"
  :autoload pinyinlib-build-regexp-string
  :init
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

(use-package vertico
  :custom
  (vertico-count 15)
  (vertico-resize nil)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; :hook ((after-init . vertico-mode)
  :hook ((doom-first-input . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy)))

;; (when (childframe-completion-workable-p)
;;  (use-package vertico-posframe
;;    :hook (vertico-mode . vertico-posframe-mode)
;;    :init (setq vertico-posframe-poshandler
;;                #'posframe-poshandler-frame-center-near-bottom
;;                vertico-posframe-parameters
;;                '((left-fringe  . 8)
;;                  (right-fringe . 8))))
;; )

(use-package nerd-icons-completion
  ;; :after vertico)
  :hook (vertico-mode . nerd-icons-completion-mode))

(use-package marginalia
  :hook (after-init . marginalia-mode))
  ;; :hook (doom-first-input . marginalia-mode))

(defun albert/consult-rg-current-dir ()
  "Runs `consult-rg' against the current buffer's directory."
  (interactive)
  (let (my-current-dir (file-name-directory (buffer-file-name)))
    (if (stringp my-current-dir)
        (consult-ripgrep (file-name-directory (buffer-file-name)) nil)
      (consult-ripgrep default-directory nil)
      )))

(defun albert/consult--dominating-file (file &optional dir)
  "Look up directory hierarchy for FILE, starting in DIR.
Like `locate-dominating-file', but DIR defaults to
`default-directory' and the return value is expanded."
  (and (setq dir (locate-dominating-file (or dir default-directory) file))
       (expand-file-name dir)))

(defun albert/consult--git-root ()
  "Return root of current project or nil on failure.
Use the presence of a \".git\" file to determine the root."
  (albert/consult--dominating-file ".git"))

(defun albert/consult-rg-project-root (&optional query)
  "Not documented, QUERY."
  (interactive)
  (let ((rootdir (albert/consult--git-root)))
    (unless rootdir
      (error "Could not find the project root. Create a git, hg, or svn repository there first"))
    (consult-ripgrep rootdir nil)))

(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h"   . consult-history)
         ("C-c k"   . consult-kmacro)
         ("C-c m"   . consult-man)
         ("C-c i"   . consult-info)
         ("C-c r"   . consult-ripgrep)
         ("C-c T"   . consult-theme)
         ("C-."     . consult-imenu)

         ("C-c c e" . consult-colors-emacs)
         ("C-c c w" . consult-colors-web)
         ("C-c c f" . describe-face)
         ("C-c c l" . find-library)
         ("C-c c t" . consult-theme)

         ([remap Info-search]        . consult-info)
         ([remap isearch-forward]    . consult-line)
         ([remap recentf-open-files] . consult-recent-file)
         ("C-c n"                    . albert/consult-recent-file)

         ;; Super键就是macOS里改键后的Alt键，即Command键
         ("s-f"     . albert/consult-rg-current-dir)
         ("s-r"     . albert/consult-rg-project-root)

         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b"   . consult-buffer)              ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#"     . consult-register-load)
         ("M-'"     . consult-register-store)        ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#"   . consult-register)
         ;; Other custom bindings
         ("M-y"     . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e"   . consult-compile-error)
         ("M-g f"   . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g"   . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o"   . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m"   . consult-mark)
         ("M-g k"   . consult-global-mark)
         ("M-g i"   . consult-imenu)
         ("M-g I"   . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d"   . consult-find)
         ("M-s D"   . consult-locate)
         ("M-s g"   . consult-grep)
         ("M-s G"   . consult-git-grep)
         ("M-s r"   . consult-ripgrep)
         ("M-s l"   . consult-line)
         ("C-s"     . consult-line)
         ("M-s L"   . consult-line-multi)
         ("M-s k"   . consult-keep-lines)
         ("M-s u"   . consult-focus-lines)
         ;; Isearch integration
         ("M-s e"   . consult-isearch-history)
         :map isearch-mode-map
         ("M-e"     . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s e"   . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l"   . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L"   . consult-line-multi)            ;; needed by consult-line to detect isearch

         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  (defface consult-file
    '((t :inherit font-lock-comment-face))
    "Face used to highlight files in `consult-buffer'.")

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))

  ;; More utils
  (defvar consult-colors-history nil
    "History for `consult-colors-emacs' and `consult-colors-web'.")

  ;; No longer preloaded in Emacs 28.
  (autoload 'list-colors-duplicates "facemenu")
  ;; No preloaded in consult.el
  (autoload 'consult--read "consult")

  (defun consult-colors-emacs (color)
    "Show a list of all supported colors for a particular frame.

You can insert the name (default), or insert or kill the hexadecimal or RGB
value of the selected COLOR."
    (interactive
     (list (consult--read (list-colors-duplicates (defined-colors))
                          :prompt "Emacs color: "
                          :require-match t
                          :category 'color
                          :history '(:input consult-colors-history))))
    (insert color))

  ;; Adapted from counsel.el to get web colors.
  (defun consult-colors--web-list nil
    "Return list of CSS colors for `counsult-colors-web'."
    (require 'shr-color)
    (sort (mapcar #'downcase (mapcar #'car shr-color-html-colors-alist)) #'string-lessp))

  (defun consult-colors-web (color)
    "Show a list of all CSS colors.\

You can insert the name (default), or insert or kill the hexadecimal or RGB
value of the selected COLOR."
    (interactive
     (list (consult--read (consult-colors--web-list)
                          :prompt "Color: "
                          :require-match t
                          :category 'color
                          :history '(:input consult-colors-history))))
    (insert color))

  (defun albert/consult-recent-file ()
    "Find recent file using `completing-read'. 去掉预览"
    (interactive)
    (find-file
     (consult--read
      (or
       (mapcar #'consult--fast-abbreviate-file-name (bound-and-true-p recentf-list))
       (user-error "No recent files, `recentf-mode' is %s"
                   (if recentf-mode "enabled" "disabled")))
      :prompt "My Find recent file: "
      :sort nil
      :require-match t
      :category 'file
      :state nil
      :history 'file-name-history)))
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  (setq consult-preview-key nil)
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-line consult-line-multi :preview-key 'any
   consult-buffer consult-recent-file consult-theme :preview-key '(:debounce 1.0 any)
   consult-goto-line :preview-key '(:debounce 0.5 any)
   consult-ripgrep consult-git-grep consult-grep
   ;; :initial (selected-region-or-symbol-at-point)
   :preview-key '(:debounce 0.5 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help))

;; (use-package consult-flyspell
;;   :bind ("M-g s" . consult-flyspell))

;; (use-package consult-yasnippet
;;   :bind ("M-g y" . consult-yasnippet))

;; https://karthinks.com/software/fifteen-ways-to-use-embark/ 这个写得不错
(use-package embark
  :bind (("s-."   . embark-act)
         ("C-s-." . embark-act)
         ("M-."   . embark-dwim)        ; overrides `xref-find-definitions'
         ([remap describe-bindings] . embark-bindings)
         :map minibuffer-local-map
         ;; C-o 和ivy一样了
         ("C-o" . embark-act)
         ("M-." . my-embark-preview)
         ;; https://midirus.com/blog/from-ivy-to-vertico
         :map vertico-map
         ;; Use page-up/down to scroll vertico buffer, like ivy does by default.
         ("<prior>" . 'vertico-scroll-down)
         ("<next>"  . 'vertico-scroll-up))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; (define-key minibuffer-local-map (kbd "n") 'vertico-next)

  ;; Manual preview for non-Consult commands using Embark
  (defun my-embark-preview ()
    "Previews candidate in vertico buffer, unless it's a consult command."
    (interactive)
    (unless (bound-and-true-p consult--preview-function)
      (save-selected-window
        (let ((embark-quit-after-action nil))
          (embark-dwim)))))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (with-eval-after-load 'which-key
    (defun embark-which-key-indicator ()
      "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
      (lambda (&optional keymap targets prefix)
        (if (null keymap)
            (which-key--hide-popup-ignore-command)
          (which-key--show-keymap
           (if (eq (plist-get (car targets) :type) 'embark-become)
               "Become"
             (format "Act on %s '%s'%s"
                     (plist-get (car targets) :type)
                     (embark--truncate-target (plist-get (car targets) :target))
                     (if (cdr targets) "…" "")))
           (if prefix
               (pcase (lookup-key keymap prefix 'accept-default)
                 ((and (pred keymapp) km) km)
                 (_ (key-binding prefix 'accept-default)))
             keymap)
           nil nil t (lambda (binding)
                       (not (string-suffix-p "-argument" (cdr binding))))))))

    (setq embark-indicators
          '(embark-which-key-indicator
            embark-highlight-indicator
            embark-isearch-highlight-indicator))

    (defun embark-hide-which-key-indicator (fn &rest args)
      "Hide the which-key indicator immediately when using the completing-read prompter."
      (which-key--hide-popup-ignore-command)
      (let ((embark-indicators
             (remq #'embark-which-key-indicator embark-indicators)))
        (apply fn args)))

    (advice-add #'embark-completing-read-prompter
                :around #'embark-hide-which-key-indicator)))

(use-package embark-consult
  :bind (:map minibuffer-mode-map
              ("C-c C-o" . embark-export))
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Auto completion
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-preview-current nil)
  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay '(0.4 . 0.2))
  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  :bind ("M-/" . completion-at-point)
  ;; :hook ((after-init . global-corfu-mode)
  :hook ((doom-first-input . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode)))

;; (unless (display-graphic-p)
;;   (use-package corfu-terminal
;;     :hook (global-corfu-mode . corfu-terminal-mode)))

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Add extensions
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev))

(use-package amx
  :ensure nil
  :bind (("M-x" . amx))
  :load-path "~/.emacs.d/site-lisp/extensions/amx"
  :config
  (setq amx-history-length 50
        amx-backend 'consult))

(recentf-mode +1)

(provide 'init-consult)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-consult.el ends here
