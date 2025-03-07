;;; init-lang.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; linux的term
(if (eq system-type 'gnu/linux)
  (global-set-key [?\C-c ?\C-_] 'comment-or-uncomment-region))

(when (or IS-WINDOWS (eq window-system 'x) (eq system-type 'darwin))
  (global-set-key [?\C-c ?\C-/] 'comment-or-uncomment-region))

(defun my-comment-or-uncomment-region (beg end &optional arg)  
  (interactive (if (use-region-p)  
                   (list (region-beginning) (region-end) nil)  
                   (list (line-beginning-position)  
                       (line-beginning-position 2))))  
  (comment-or-uncomment-region beg end arg)  
)  
(global-set-key [remap comment-or-uncomment-region] 'my-comment-or-uncomment-region)

(setq c-default-style "linux")
(setq c-basic-offset 4)

(setq default-tab-width 4)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; (setq tab-width 4)
;; (setq tab-stop-list ())

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; (setq python-shell-interpreter "python"
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

;; (defun albert|python-shell-send-buffer (&optional send-main msg)
;;   "Send the entire buffer to inferior Python process.
;; When optional argument SEND-MAIN is non-nil, allow execution of
;; code inside blocks delimited by \"if __name__== \\='__main__\\=':\".
;; When called interactively SEND-MAIN defaults to nil, unless it's
;; called with prefix argument.  When optional argument MSG is
;; non-nil, forces display of a user-friendly message if there's no
;; process running; defaults to t when called interactively."
;;   (interactive (list current-prefix-arg t))
;;   (save-restriction
;;     (widen)
;;     (set-language-environment 'Chinese-GB18030)
;;     (python-shell-send-region (point-min) (point-max) send-main msg)
;;     (set-language-environment 'UTF-8)))

;; (add-hook 'python-mode-hook
;;   (lambda ()
;;     (define-key python-mode-map (kbd "C-c C-c") 'albert|python-shell-send-buffer)))

;; [2019-08-05 周一 17:00:08] ipython使用cp936，即GBK编码，中文显示终于正常了。上面的hook无效。
;; search keyword: process 中文, https://emacs-china.org/t/emacs-shell/2730/2

;; (setq process-coding-system-alist (cons '("ipython" . (cp936 . cp936)) process-coding-system-alist))

(when IS-WINDOWS
  (dolist (cmd '("ipython" "python" "ping"))
    (setq process-coding-system-alist (cons (cons cmd '(cp936 . cp936))
                                            process-coding-system-alist))))

(use-package py-autopep8
  :ensure t
  :defer t
  :init
    (add-hook 'python-mode-hook 'py-autopep8-mode))
    ;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

(use-package cmuscheme
  :ensure nil
  :defer t)
(setq scheme-program-name "racket")         ;; 如果用 Petite 就改成 "petite"
;; (setq scheme-program-name "scheme")         ;; 如果用 Petite 就改成 "petite"

;; bypass the interactive question and start the default interpreter
(defun scheme-proc ()
  "Return the current Scheme process, starting one if necessary."
  (unless (and scheme-buffer
	 (get-buffer scheme-buffer)
	 (comint-check-proc scheme-buffer))
    (save-window-excursion
(run-scheme scheme-program-name)))
  (or (scheme-get-process)
(error "No current process. See variable `scheme-buffer'")))

(defun scheme-split-window ()
  (cond
   ((= 1 (count-windows))
    (delete-other-windows)
    ;; (split-window-vertically (floor (* 0.68 (window-height))))
    (split-window-horizontally (floor (* 0.60 (window-width))))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window 1))
   ((not (cl-find "*scheme*"
	 (mapcar (lambda (w) (buffer-name (window-buffer w)))
		 (window-list))
	 :test 'equal))
    (other-window 1)
    (switch-to-buffer "*scheme*")
    (other-window -1))))

(defun scheme-send-last-sexp-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-last-sexp))

(defun scheme-send-definition-split-window ()
  (interactive)
  (scheme-split-window)
  (scheme-send-definition))

(add-hook 'scheme-mode-hook
  (lambda ()
    (paredit-mode 1)
    (evil-paredit-mode 1)
    (define-key scheme-mode-map (kbd "<f6>") 'scheme-send-last-sexp-split-window)
    (define-key scheme-mode-map (kbd "<f7>") 'scheme-send-definition-split-window)))

;; homepage: https://web-mode.org/
(use-package web-mode
  :ensure t
  :defer t
  :mode (("\\.html" . web-mode)
         ("\\.htm" . web-mode))
  :config
  ;; [2021-07-16 Fri 11:10:54]
  ;; d/ -> <div></div>
  ;; s/ -> <span></span>
  (setq web-mode-enable-auto-expanding t))

;; 默认在 .emacs.d/tree-sitter 目录，文件名格式：libtree-sitter-python.dylib -> ~/.local/share/nvim/lazy/nvim-treesitter/parser/python.so
;; nvim中nvim-treesitter plugin, TSInstall java
;; 也可以设置 (setq treesit-extra-load-path '("~/.local/share/nvim/lazy/nvim-treesitter/parser"))
(use-package treesit-auto
  :hook (after-init . global-treesit-auto-mode)
  :config (setq treesit-auto-install 'prompt))

(defvar-local albert/treesit--inspect-name nil
  "Used by `albert-treesit-inspect-mode' to show node name in mode-line.")

;; deepseek牛逼 [2025-03-06 Thu 17:59:21]
(defun albert/get-json-node-hierarchy ()
  "Get the path from the root node to the current JSON node at point, including keys."
  (interactive)
  (when (derived-mode-p 'json-ts-mode)
    (let* ((node (treesit-node-at (point)))
           (path ()))
      (while node
        (let ((node-type (treesit-node-type node)))
          (if (string= node-type "pair")  ;; 如果是键值对
              (let ((key-node (treesit-node-child node 0)))  ;; 获取键节点
                (push (string-trim (treesit-node-text key-node) "\"" "\"") path))  ;; 将键名加入路径
            ))
        (setq node (treesit-node-parent node)))

      (setq albert/treesit--inspect-name (mapconcat 'identity path "."))
      )))

(define-minor-mode albert-treesit-inspect-mode
  "Minor mode that displays in the mode-line the node which starts at point.

doom-modeline要用Minor mode才能在按键后刷新modeline? 依赖tree-sitter-mode，如果

报错，需要检查tree-sitter-mode是否enabled。

When this mode is enabled, the mode-line displays

thing-name or config-name for json and yaml file.

If no node starts at point, i.e., point is in the middle of a
node, then the mode line displays the earliest node that spans point,
and its immediate parent.
"
  :lighter nil
  (if albert-treesit-inspect-mode
      (progn
        (add-hook 'post-command-hook
                  #'albert/get-json-node-hierarchy)
        (add-to-list 'mode-line-misc-info
                     '(:eval albert/treesit--inspect-name)))
    (remove-hook 'post-command-hook
                 #'albert/get-json-node-hierarchy)
    (setq mode-line-misc-info
          (remove '(:eval albert/treesit--inspect-name)
                  mode-line-misc-info))))

(provide 'init-lang)
