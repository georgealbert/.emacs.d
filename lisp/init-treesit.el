;;; init-treesit.el -*- lexical-binding: t; no-byte-compile: t; -*-

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

(provide 'init-treesit)
