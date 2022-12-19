;;; init-tree-sitter.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; --- Configure for tree sitter

(use-package tree-sitter
  ;; :defer 3
  :hook ((prog-mode . tree-sitter-mode)
         (yaml-mode . tree-sitter-mode))
  :config
  ;; (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

  (use-package tree-sitter-langs)

  ;; https://github.com/manateelazycat/lazycat-emacs/blob/master/site-lisp/config/init-tree-sitter.el

  ;; Add Emacs-Lisp for tree-sitter:
  ;;
  ;; 1. git clone https://github.com/Wilfred/tree-sitter-elisp
  ;; 2. cd ~/workspace/tree-sitter-elisp && gcc ./src/parser.c -fPIC -I./ -I./src/ --shared -o elisp.dylib
  ;; 3. cp -p ./elisp.dylib ~/.emacs.d/elpa/tree-sitter-langs-*/bin
  (tree-sitter-load 'elisp)
  (add-to-list 'tree-sitter-major-mode-language-alist '(emacs-lisp-mode . elisp))
  (add-to-list 'tree-sitter-major-mode-language-alist '(inferior-emacs-lisp-mode . elisp))

  ;; Add Vue for tree-sitter:
  ;;
  ;; 1. git clone git@github.com:ikatyang/tree-sitter-vue.git
  ;; 2. cd ~/workspace/tree-sitter-vue && g++ ./src/parser.c ./src/scanner.cc -fPIC -I./ -I./src -v --shared -o vue.dylib
  ;; 3. cp -p ./vue.dylib ~/.emacs.d/elpa/tree-sitter-langs-*/bin (~/.tree-sitter-langs/bin is path of your tree-sitter-langs repo)
  (tree-sitter-load 'vue)
  (add-to-list 'tree-sitter-major-mode-language-alist '(web-mode . vue))

  ;; Add Typescript for tree-sitter.
  ;;
  ;; 1. git clone https://github.com/tree-sitter/tree-sitter-typescript.git
  ;; 2. gcc ./tsx/src/parser.c ./tsx/src/scanner.c -fPIC -I./ --shared -o typescript.so
  ;; 3. cp ./typescript.so ~/.tree-sitter-langs/bin (~/.tree-sitter-langs/bin is path of your tree-sitter-langs repo)
  ;; (tree-sitter-load 'typescript)
  ;; (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-mode . typescript))

  (defvar meain/tree-sitter-config-nesting--queries '((json-mode . "(object (pair (string (string_content) @key) (_)) @item)")
                                                      (yaml-mode . "(block_mapping_pair (flow_node) @key (_)) @item")
                                                      (nix-mode . "(bind (attrpath (attr_identifier) @key)) @item")))

  (defun meain/tree-sitter-config-nesting ()
    (when-let* ((query-s (cdr (assq major-mode meain/tree-sitter-config-nesting--queries)))
                (query (tsc-make-query tree-sitter-language query-s))
                (root-node (tsc-root-node tree-sitter-tree))
                (matches (tsc-query-matches query root-node #'tsc--buffer-substring-no-properties)))
      (string-join
       (cl-remove-if #'null
                     (seq-map (lambda (x)
                                (let* ((item (seq-elt (cdr x) 0))
                                       (key (seq-elt (cdr x) 1))
                                       (pos (tsc-node-byte-range (cdr item))))
                                  (when (> (byte-to-position (cdr pos))
                                           (point)
                                           (byte-to-position (car pos)))
                                    (format "%s" (tsc-node-text (cdr key))))))
                              matches))
       ".")))

  (defun meain/get-config-nesting-paths ()
    "Get out all the nested paths in a config file."
    (when-let* ((query-s (cdr (assq major-mode meain/tree-sitter-config-nesting--queries)))
                (root-node (tsc-root-node tree-sitter-tree))
                (query (tsc-make-query tree-sitter-language query-s))
                (matches (tsc-query-matches query root-node #'tsc--buffer-substring-no-properties))
                (item-ranges (seq-map (lambda (x)
                                        (let ((item (seq-elt (cdr x) 0))
                                              (key (seq-elt (cdr x) 1)))
                                          (list (tsc-node-text (cdr key))
                                                (tsc-node-range (cdr key))
                                                (tsc-node-range (cdr item)))))
                                      matches))
                (parent-nodes '(("#" 0))))
      (mapcar (lambda (x)
                (let* ((current-end (seq-elt (cadr (cdr x)) 1))
                       (parent-end (cadar parent-nodes))
                       (current-key (car x)))
                  (progn
                    (if (> current-end parent-end)
                        (setq parent-nodes
                              (-filter (lambda (y) (< current-end (cadr y)))
                                       parent-nodes)))
                    (setq parent-nodes (cons (list current-key current-end) parent-nodes))
                    (list (reverse (mapcar #'car parent-nodes))
                          (seq-elt (cadr x) 0)))))
              item-ranges)))

  (defun meain/imenu-config-nesting-path ()
    "Return config-nesting paths for use in imenu"
    (mapcar (lambda (x)
              (cons (string-join (car x) ".") (cadr x)))
            (meain/get-config-nesting-paths)))

  (setq meain/tree-sitter-class-like '((rust-mode . (impl_item))
                                       (python-mode . (class_definition))))

  (setq meain/tree-sitter-function-like '((rust-mode . (function_item))
                                          (go-mode . (function_declaration method_declaration))
                                          (sh-mode . (function_definition))
                                          (python-mode . (function_definition))))

  (defun meain/tree-sitter-thing-name (kind)
    "Get name of tree-sitter KIND thing."
    (when-let (tree-sitter-mode
               (node-types (pcase kind
                             ('class-like meain/tree-sitter-class-like)
                             ('function-like meain/tree-sitter-function-like)))
               (node-at-point (cl-some #'tree-sitter-node-at-point
                                       (alist-get major-mode node-types)))
               (node-name (tsc-get-child-by-field node-at-point :name)))
      (tsc-node-text node-name)))
  ;; Connect to which-function for magit-log-trace-definition
  (setq which-func-functions
        (list
         (lambda () (meain/tree-sitter-thing-name 'function-like))
         (lambda () (meain/tree-sitter-thing-name 'class-like))))
  )

;; (use-package tree-sitter-langs
;;   :after tree-sitter)

(use-package which-func
  :commands (which-function))

;; https://blog.meain.io/2022/navigating-config-files-using-tree-sitter/
(use-package json-mode
  :defer t
  :config
  (add-hook 'json-mode-hook (lambda ()
                              (setq imenu-create-index-function #'meain/imenu-config-nesting-path))))

(use-package yaml-mode
  :defer t
  ;; :disabled t
  :config
  (remove-hook 'yaml-mode-hook 'yaml-set-imenu-generic-expression) ;; don't use default one
  (add-hook 'yaml-mode-hook (lambda ()
                              (setq imenu-create-index-function #'meain/imenu-config-nesting-path))))

(defvar-local albert/treesit--inspect-name nil
  "Used by `albert-treesit-inspect-mode' to show node name in mode-line.")

(defun albert/tree-sitter-thing-name ()
  (if (boundp 'tree-sitter-mode)
      (let ((thing-name (meain/tree-sitter-thing-name 'function-like))
            (config-nesting (meain/tree-sitter-config-nesting)))
        (if thing-name
            (setq albert/treesit--inspect-name (format "methond:%s" thing-name))
          (if config-nesting
              (setq albert/treesit--inspect-name (format "%s" config-nesting)))))
    (when-let (func-name (which-function))
      (setq albert/treesit--inspect-name (format ":%s" func-name))))

  ;; 貌似这行有了才行，哈哈
  (force-mode-line-update))

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
        (unless tree-sitter-mode
                   (tree-sitter-mode))
        (add-hook 'post-command-hook
                  #'albert/tree-sitter-thing-name)
        (add-to-list 'mode-line-misc-info
                     '(:eval albert/treesit--inspect-name)))
    (remove-hook 'post-command-hook
                 #'albert/tree-sitter-thing-name)
    (setq mode-line-misc-info
          (remove '(:eval albert/treesit--inspect-name)
                  mode-line-misc-info))))

(provide 'init-tree-sitter)
