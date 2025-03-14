;;; init-ivy.el -*- lexical-binding: t; -*-

(defun albert-ivy-done()
  "ivy的tab键优化. 只按一次<tab>就补全目录，和helm一样."
  (interactive)
  (let ((dir ivy--directory))
    (ivy-partial-or-done)
    (when (string= dir ivy--directory)
      (ivy-insert-current)
      (when (and (eq (ivy-state-collection ivy-last) #'read-file-name-internal)
                 (setq dir (ivy-expand-file-if-directory (ivy-state-current ivy-last))))
        (ivy--cd dir)
        (setq this-command 'ivy-cd)))))

;; C-o f to toggle case sensitive, @see https://github.com/abo-abo/swiper/issues/1104
(defun re-builder-extended-pattern (str)
  (let* ((len (length str)))
    (cond
     ((<= (length str) 0)
      (ivy--regex-plus str)
      )

     ;; If the first charater of input in ivy is ":",
     ;; remaining input is converted into Chinese pinyin regex.
     ((string= (substring str 0 1) ":")
      (require 'pinyinlib)
      ;; pinyinlib-build-regexp-string加上最后2个参数 "nil t"，只搜索简体中文，不包括开头的首字母。
      (let ((str (pinyinlib-build-regexp-string (substring str 1 len) t nil t)))
        ;; 拼音不要按字模糊匹配，否则太慢了。
        (ivy--regex-ignore-order str)
        ;; (ivy--regex str)
        ))

     ;; If the first charater of input in ivy is "/",
     ;; remaining input is converted to pattern to search camel case word
     ;; For example, input "/ic" match "isController" or "isCollapsed"
     ((string= (substring str 0 1) "/")
      (let* ((rlt "")
             (i 0)
             (subs (substring str 1 len))
             c)
        (when (> len 2)
          (setq subs (upcase subs))
          (while (< i (length subs))
            (setq c (elt subs i))
            (setq rlt (concat rlt (cond
                                   ((and (< c ?a) (> c ?z) (< c ?A) (> c ?Z))
                                    (format "%c" c))
                                   (t
                                    (concat (if (= i 0) (format "[%c%c]" (+ c 32) c)
                                              (format "%c" c))
                                            "[a-z]+")))))
            (setq i (1+ i))))
        (setq str rlt)
        (ivy--regex-plus str)
        ))

     (t
      ;; 不能用ivy--regex-ignore-order，否则英文用空格分隔时的结果，match的高亮不对。
      (ivy--regex-plus str)))))
    ;; (ivy--regex-ignore-order str)))


(defun my-extended-regexp (str)
  "Build regex compatible with pinyin from STR.

https://github.com/redguardtoo/emacs.d/blob/55a3b4fe9bc981de51853af49754fa4c85d86c87/lisp/init-utils.el#L568
"
  (let* ((len (length str))
         bn)
    (cond
     ;; do nothing
     ((<= (length str) 1))

     ;; If the first character of input in ivy is ":" or ";",
     ;; remaining input is converted into Chinese pinyin regex.
     ((or (and (string-match "[:\|;]" (substring str 0 1))
               (setq str (substring str 1 len)))
          (and (setq bn (buffer-name))
               (or (member bn '("*Org Agenda*"))
                   (string-match ".*EMMS Playlist\\|\\.org$" bn))))
      (require 'pinyinlib)
      (setq str (pinyinlib-build-regexp-string str)))

     ;; If the first character of input in ivy is "/",
     ;; remaining input is converted to pattern to search camel case word
     ;; For example, input "/ic" match "isController" or "isCollapsed"
     ((string= (substring str 0 1) "/")
      (let* ((rlt "")
             (i 0)
             (subs (substring str 1 len))
             c)
        (when (> len 2)
          (setq subs (upcase subs))
          (while (< i (length subs))
            (setq c (elt subs i))
            (setq rlt (concat rlt (cond
                                   ((and (< c ?a) (> c ?z) (< c ?A) (> c ?Z))
                                    (format "%c" c))
                                   (t
                                    (concat (if (= i 0) (format "[%c%c]" (+ c 32) c)
                                              (format "%c" c))
                                            "[a-z]+")))))
            (setq i (1+ i))))
        (setq str rlt))))
    str))

(defun my-re-builder-extended-pattern (str)
  "Build regex compatible with pinyin from STR."
  (ivy--regex-plus (my-extended-regexp str)))

;; doc: https://zhuanlan.zhihu.com/p/67307599
;;      pinyinlib搜索中文首字母开头的中文，按26个字母，每个字母有几个中文
;;      :搜索，!排除
;;      :zw !by
;; homepage: https://github.com/cute-jumper/pinyinlib.el
(use-package pinyinlib
  :defer t
  :load-path "~/.emacs.d/site-lisp/extensions/pinyinlib")

(use-package ivy
  :defer t
  :diminish ivy-mode counsel-mode
  :bind
  (:map ivy-minibuffer-map
        ("<tab>" . albert-ivy-done))
  :config
  ;; The default sorter is much to slow and the default for `ivy-sort-max-size'
  ;; is way too big (30,000). Turn it down so big repos affect project
  ;; navigation less.
  (setq ivy-sort-max-size 7500)

  ;; Counsel changes a lot of ivy's state at startup; to control for that, we
  ;; need to load it as early as possible. Some packages (like `ivy-prescient')
  ;; require this.
  (require 'counsel nil t)

  (setq ivy-height 15 ;;
        ;; 不循环，否则不知道是否到底了。
        ;; ivy-wrap t
        ivy-count-format "(%d/%d) "
        ivy-fixed-height-minibuffer t
        ;; projectile-completion-system 'ivy
        ;; disable magic slash on non-match
        ivy-magic-slash-non-match-action nil
        ;; don't show recent files in switch-buffer
        ivy-use-virtual-buffers nil

        ;; ...but if that ever changes, show their full path
        ivy-virtual-abbreviate 'full
        ;; don't quit minibuffer on delete-error
        ivy-on-del-error-function #'ignore
        ;; enable ability to select prompt (alternative to `ivy-immediate-done')
        ivy-use-selectable-prompt t)

  (setq ivy-re-builders-alist '((t . my-re-builder-extended-pattern)))
  ;; (setq ivy-re-builders-alist '(
  ;;                               ;; (counsel-evil-marks . ivy--regex-plus)
  ;;                               (t . re-builder-extended-pattern)
  ;;                               ))

  (use-package amx
    :ensure nil
    :load-path "~/.emacs.d/site-lisp/extensions/amx"
    :init
    (setq amx-history-length 50))

  (ivy-mode +1)
  )

(use-package ivy-rich
  ;; :defer 1
  :diminish
  :hook (doom-first-input . ivy-rich-mode)
  :config
  ;; 性能更好点
  (setq ivy-rich-parse-remote-buffer nil)

  (setcdr (assq t ivy-format-functions-alist)
          #'ivy-format-function-line)

  ;; 'full or 'absolute 'abbrev
  (setq ivy-rich-path-style 'full)
  (ivy-rich-mode +1)
  )

(use-package nerd-icons-ivy-rich
  :after ivy-rich
  :config
  ;; Whether display the icons
  (setq nerd-icons-ivy-rich-icon t)

  ;; Whether display the colorful icons.
  ;; It respects `nerd-icons-color-icons'.
  (setq nerd-icons-ivy-rich-color-icon t)
  (nerd-icons-ivy-rich-mode 1)
  )

(defun counsel-recent-directory (&optional n)
  "Goto recent directories.
If N is not nil, only list directories in current project."
  (interactive "P")
  (unless recentf-mode (recentf-mode 1))
  (let* ((cands (delete-dups
                 (append my-dired-directory-history
                         (mapcar 'file-name-directory recentf-list)
                         ;; fasd history
                         (if (executable-find "fasd")
                             (nonempty-lines (shell-command-to-string "fasd -ld"))))))
         (root-dir (if (ffip-project-root) (file-truename (ffip-project-root)))))
    (when (and n root-dir)
      (setq cands (delq nil (mapcar (lambda (f) (path-in-directory-p f root-dir)) cands))))
    (ivy-read "directories:" cands :action 'dired)))

(defun counsel-ag-current-dir ()
  "Runs `counsel-ag' against the current buffer's directory."
  (interactive)
  (let (my-current-dir (file-name-directory (buffer-file-name)))
    ;; (if (eq nil my-current-dir)
    (if (stringp my-current-dir)
        (counsel-ag "" (file-name-directory (buffer-file-name)))
      (counsel-ag "" default-directory)
      )))

(defun counsel-rg-current-dir ()
  "Runs `counsel-rg' against the current buffer's directory."
  (interactive)
  (let (my-current-dir (file-name-directory (buffer-file-name)))
    (if (stringp my-current-dir)
        (counsel-rg "" (file-name-directory (buffer-file-name)) nil (concat "rg (" default-directory "):"))
      (counsel-rg "" default-directory nil (concat "rg (" default-directory "):"))
      )))

;;;###autoload
(defun counsel-ag-project-root (&optional query)
  "Runs `counsel-ag' against the current project's directory."
  (interactive)
  (let ((rootdir (counsel--git-root)))
    (unless rootdir
      (error "Could not find the project root. Create a git, hg, or svn repository there first."))
    (counsel-ag "" rootdir nil (concat "ag (" rootdir "):"))))

(defun counsel-rg-project-root (&optional query)
  "Runs `counsel-rg' against the current project's directory."
  (interactive)
  (let ((rootdir (counsel--git-root)))
    (unless rootdir
      (error "Could not find the project root. Create a git, hg, or svn repository there first."))
    (counsel-rg "" rootdir nil (concat "rg (" rootdir "):"))))

(use-package counsel
  :diminish
  :bind (
         ("M-x" . counsel-M-x)
         ("C-c n" . counsel-buffer-or-recentf)
         ("s-r" . counsel-rg-project-root)
         ("s-f" . counsel-rg-current-dir)
         ("C-x C-f" . counsel-find-file))
  :init
  (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers
  :config
  ;; Integrate with `helpful'
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable))

;; https://oremacs.com/2019/04/07/swiper-isearch/
;; https://pengpengxp.github.io/emacs/emacs_isearch_summary.html
(global-set-key (kbd "C-s") 'swiper-isearch)

(use-package ivy-posframe
  :after ivy
  :disabled t
  :config
  (setq ivy-posframe-height-alist '((swiper . 15)
                                    (t      . 20)))
  (setq ivy-posframe-display-functions-alist
        '((swiper          . ivy-display-function-fallback)
          (swiper-isearch  . ivy-display-function-fallback)
          (complete-symbol . ivy-posframe-display-at-point)
          (counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
          (t               . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-mode 1))

;; Auto completion
;; https://github.com/minad/corfu
;; Supports the Orderless completion style. The filter string can contain arbitrary characters, after inserting a space via M-SPC (configurable via corfu-quit-at-boundary and corfu-separator).
;; 按 M-SPC分割，但是需要consult。ivy不支持，唉
(use-package corfu
  :disabled t
  ;; :custom
  ;; (corfu-auto t)
  ;; (corfu-auto-prefix 2)
  ;; (corfu-preview-current nil)
  ;; (corfu-auto-delay 0.2)
  ;; (corfu-popupinfo-delay '(0.4 . 0.2))
  ;; TODO: 为什么设置了global-corfu-modes不在lsp-bridge-mode时激活，整个corfu就不能自动补全了呢？
  ;; (global-corfu-modes '((not lsp-bridge-mode)))
  :config
  (setq corfu-auto t
        corfu-auto-prefix 2
        corfu-auto-delay 0.2
        corfu-popupinfo-delay '(0.4 . 0.2)
        ;; global-corfu-modes '((not
        ;;                       lsp-bridge-mode
        ;;                       circe-mode
        ;;                       help-mode
        ;;                       gud-mode)
        ;;                      t)
        ;; corfu-preselect 'prompt
        ;; corfu-count 16
        ;; corfu-max-width 120
        ;; corfu-preview-current 'insert
        ;; tab-always-indent 'complete
        )
  ;; :init
  ;; (setopt global-corfu-modes '((not lsp-bridge-mode)))
  ;; :custom-face
  ;; (corfu-border ((t (:inherit region :background unspecified))))
  :bind ("M-/" . completion-at-point) ;; TODO: 可能是给terminal中用minibuffer补全时使用的，在GUI中，总是吞掉之前输入的目录
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode)
         ))

;; (use-package nerd-icons-corfu
;;   :after corfu
;;   :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Add extensions
(use-package cape
  ;; :bind ("M-/" . completion-at-point)
  :disabled t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev))

(provide 'init-ivy)
