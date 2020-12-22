;; https://archive.casouri.cat/note/2020/painless-transition-to-portable-dumper/index.html
;; https://emacs-china.org/t/portable-dumper/11584/6

;; 1. 生成dump文件
;; C:\Users\Albert>emacs --batch -q -l e:\home\albert\.emacs.d\dump.el
;; dumping fingerprint: f4adb1e0017f5df19d785c7c96ff758ba46ae5278560ece5370b38770add6028
;; Dump complete
;; Byte counts: header=80 hot=20850528 discardable=128688 cold=11993320
;; Reloc counts: hot=1240019 discardable=4980

;; 2. 启动emacs时加载dump文件
;; C:\Users\Albert>emacs --dump-file="e:/home/albert/.emacs.d/emacs.pdmp"

(require 'package)

;; load autoload files and populate load-paths
(package-initialize)

(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/my_elisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/my_elisp"))

;; store load-path
(setq albert-dumped-load-path load-path
      albert-dumped t)

(defvar org-modules
  '(org-tempo))

;; (package-initialize) doenst require each package, we need to load
;; those we want manually
(dolist (package '(use-package org ;; org-crypt
                    benchmark-init
                    ;; gcmh
                    general
                    doom-themes doom-deeper-blue-theme
                    doom-modeline ;; all-the-icons
                    ;; elec-pair
                    ;; highlight-parentheses
                    ;; 好像dump了以后，切窗口不正常
                    ;; winum
                    helm helm-elisp helm-ag helm-swoop
                    evil
                    ;; evil-maps
                    ;; magit
                    ;; lsp-python-ms
                    ;; lsp-mode
                    ivy counsel swiper pinyinlib find-file-in-project
                    ;; awesome-pair
                    ))
  (require package))

(global-undo-tree-mode -1)

;; the two flags are no-confirm and no-enable
(load-theme 'doom-deeper-blue t t)

;; dump image
(dump-emacs-portable "~/.emacs.d/emacs.pdmp")
