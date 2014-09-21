;; This sets up the load path so that we can override it
(package-initialize nil)
;; Override the packages with the git version of Org and other packages
(add-to-list 'load-path "~/.emacs.d/lisp/org-8.2.7c/lisp")
(add-to-list 'load-path "~/.emacs.d/lisp/org-8.2.7c/contrib/lisp")
;; Load the rest of the packages
(package-initialize t)
(setq package-enable-at-startup nil)
(require 'org)
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "~/.emacs.d/Albert.org"))
