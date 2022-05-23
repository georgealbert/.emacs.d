;;; init-tree-sitter.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; --- Configure for tree sitter

(use-package tree-sitter
  ;; :defer 3
  :hook (prog-mode . tree-sitter-mode)
  :config
  ;; (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

  ;; https://github.com/manateelazycat/lazycat-emacs/blob/master/site-lisp/config/init-tree-sitter.el

  ;; Add Emacs-Lisp for tree-sitter:
  ;;
  ;; 1. git clone https://github.com/Wilfred/tree-sitter-elisp
  ;; 2. cd ~/workspace/tree-sitter-elisp && gcc ./src/parser.c -fPIC -I./ --shared -o elisp.dylib
  ;; 3. cp -p ./elisp.dylib ~/.emacs.d/elpa/tree-sitter-langs-*/bin
  (tree-sitter-load 'elisp)
  (add-to-list 'tree-sitter-major-mode-language-alist '(emacs-lisp-mode . elisp))
  (add-to-list 'tree-sitter-major-mode-language-alist '(inferior-emacs-lisp-mode . elisp))

  ;; Add Vue for tree-sitter:
  ;;
  ;; 1. git clone git@github.com:ikatyang/tree-sitter-vue.git
  ;; 2. g++ ./src/parser.c ./src/scanner.cc -fPIC -I./ -I./src -v --shared -o vue.dylib
  ;; 3. cp -p ./vue.dylib ~/.emacs.d/elpa/tree-sitter-langs-*/bin (~/.tree-sitter-langs/bin is path of your tree-sitter-langs repo)
  (tree-sitter-load 'vue)
  (add-to-list 'tree-sitter-major-mode-language-alist '(web-mode . vue))

  ;; Add Typescript for tree-sitter.
  ;;
  ;; 1. git clone https://github.com/tree-sitter/tree-sitter-typescript.git
  ;; 2. gcc ./tsx/src/parser.c ./tsx/src/scanner.cc -fPIC -I./ --shared -o typescript.so
  ;; 3. cp ./typescript.so ~/.tree-sitter-langs/bin (~/.tree-sitter-langs/bin is path of your tree-sitter-langs repo)
  ;; (tree-sitter-load 'typescript)
  ;; (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-mode . typescript))
  )

(use-package tree-sitter-langs
  :after tree-sitter)

(provide 'init-tree-sitter)
