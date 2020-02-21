;;; init-ivy.el -*- lexical-binding: t; -*-

;; {{  C-o f to toggle case sensitive, @see https://github.com/abo-abo/swiper/issues/1104
(defun re-builder-extended-pattern (str)
  (let* ((len (length str)))
    (cond
     ;; do nothing
     ((<= (length str) 0)
      (ivy--regex-plus str)
      )

     ;; If the first charater of input in ivy is ":",
     ;; remaining input is converted into Chinese pinyin regex.
     ((string= (substring str 0 1) ":")
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
;; }}

(use-package ivy
  :defer t
  :bind
  (:map ivy-minibuffer-map
        ("<tab>" . albert-ivy-done))
  :config
  ;; Counsel changes a lot of ivy's state at startup; to control for that, we
  ;; need to load it as early as possible. Some packages (like `ivy-prescient')
  ;; require this.
  (require 'counsel nil t)

  (setq ivy-height 12
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

  ;; doc: https://zhuanlan.zhihu.com/p/67307599
  ;;      pinyinlib搜索中文首字母开头的中文，按26个字母，每个字母有几个中文
  ;;      :搜索，!排除
  ;;      :zw !by
  ;; homepage: https://github.com/cute-jumper/pinyinlib.el
  (use-package pinyinlib
      :load-path "~/.emacs.d/site-lisp/extensions/pinyinlib")
  (setq ivy-re-builders-alist '(
                                ;; (counsel-evil-marks . ivy--regex-plus)
                                (t . re-builder-extended-pattern)
                                ))
  (ivy-mode +1)
  )

(use-package ivy-rich
  :defer 1
  :config
  ;; 性能更好点
  (setq ivy-rich-parse-remote-buffer nil)

  (setcdr (assq t ivy-format-functions-alist)
          #'ivy-format-function-line)

  ;; 下面的配置是seagle0128的
  ;; Setting tab size to 1, to insert tabs as delimiters
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (setq tab-width 1)))

  ;; 'full or 'absolute 'abbrev
  (setq ivy-rich-path-style 'full)
  (ivy-rich-mode +1))

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

(use-package counsel
  :defer t
  :after (ivy)
  ;; :bind ("s-x" . counsel-M-x)
  :init
  (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers

  :config
  ;; Integrate with `helpful'
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable)
  )

(use-package ivy-posframe
  :after ivy
  :disabled t
  :custom
  (ivy-posframe-height-alist '((swiper . 15)
                               (t      . 20)))
  (ivy-posframe-display-functions-alist
   '((swiper          . nil)
     (swiper-isearch  . nil)
     (complete-symbol . ivy-posframe-display-at-point)
     (counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
     (t               . ivy-posframe-display-at-frame-center)))
  :config
  (ivy-posframe-mode 1))

(provide 'init-ivy)
