;;; init.el -*- lexical-binding: t; -*-

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; [2018-11-30 周五 11:45:50] 这个函数是必须的，否则启动报错。
;; (package-initialize nil)

(defvar albert-dumped nil
  "non-nil when a dump file is loaded (because dump.el sets this variable).")

(defmacro albert-if-dump (then &rest else)
  "Evaluate IF if running with a dump file, else evaluate ELSE."
  (declare (indent 1))
  `(if albert-dumped
       ,then
     ,@else))

(albert-if-dump
    (progn
      (message "Using pdump to start emacs...")
      (setq load-path albert-dumped-load-path)
      (global-font-lock-mode)
      (add-hook 'after-init-hook
                (lambda ()
                  (save-excursion
                    (switch-to-buffer "*scratch*")
                    (fundamental-mode)))))
  ;; add load-path’s and load autoload files
  (package-initialize))

(when window-system
  (albert-if-dump
      (enable-theme 'doom-deeper-blue)))

;; [2019-10-30 周三 17:37:50] emacs 27.0不需要了
;; (setq package-enable-at-startup nil)

;; (require 'benchmark-init-modes)
;; (require 'benchmark-init)
;; (benchmark-init/activate)

;; (defvar org-modules
;;   '(;; org-w3m
;;     ;; org-bbdb
;;     ;; org-bibtex
;;     ;; org-docview
;;     ;; org-gnus
;;     ;; org-info
;;     ;; org-irc
;;     ;; org-mhe
;;     ;; org-rmail
;;     ))

;; (require 'ob-tangle)
;; (org-babel-load-file (expand-file-name "~/.emacs.d/Albert.org"))

;; (load (expand-file-name "~/.emacs.d/Albert.el"))

;; Load path
;; Optimize: Force "lisp"" and "site-lisp" at the head to reduce the startup time.
(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "site-lisp" user-emacs-directory) load-path)
  (push (expand-file-name "lisp" user-emacs-directory) load-path))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory
          (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;; (setq comp-deferred-compilation t)

(require 'init-core)
(require 'init-editor)
(require 'init-completion)
(require 'init-ivy)
(require 'init-ui-theme)
(require 'init-ui)
(require 'init-keybinds)
(require 'init-lang)
(require 'init-tools)
(require 'init-org)
(require 'init-keyfreq)
(require 'init-editor-paren)
(require 'init-ui-treemacs)
(require 'init-hydra)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#181a26" "#FF0000" "#4eee94" "#FFFF00" "#0000cd" "#f08080" "#00FFFF" "#cccccc"])
 '(custom-safe-themes '(default))
 '(fci-rule-color "#ffe4b5")
 '(jdee-db-active-breakpoint-face-colors (cons "#100e23" "#EE82EE"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#100e23" "#4eee94"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#100e23" "#7f7f7f"))
 '(objed-cursor-color "#FF0000")
 '(package-selected-packages
   '(lsp-pyright hydra wgrep pyim find-file-in-project pinyinlib ivy-hydra ivy-prescient prescient amx smartparens cmuscheme hl-todo gcmh vimrc-mode lua-mode org-download lsp-python-ms winum treemacs-evil flycheck-posframe dired-k all-the-icons doom-modeline doom-themes py-autopep8 helm-ag ggtags go-mode web-mode use-package spinner sesman seq queue pkg-info paren-face markdown-mode magit highlight-parentheses helm-swoop evil-paredit benchmark-init))
 '(safe-local-variable-values '((encoding . UTF-8)))
 '(send-mail-function 'mailclient-send-it)
 '(vc-annotate-background "#181a26")
 '(vc-annotate-color-map
   (list
    (cons 20 "#4eee94")
    (cons 40 "#89f362")
    (cons 60 "#c3f931")
    (cons 80 "#FFFF00")
    (cons 100 "#ffe100")
    (cons 120 "#ffc300")
    (cons 140 "#FFA500")
    (cons 160 "#fa982a")
    (cons 180 "#f58c55")
    (cons 200 "#f08080")
    (cons 220 "#f55555")
    (cons 240 "#fa2a2a")
    (cons 260 "#FF0000")
    (cons 280 "#df1f1f")
    (cons 300 "#bf3f3f")
    (cons 320 "#9f5f5f")
    (cons 340 "#ffe4b5")
    (cons 360 "#ffe4b5")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-minibuffer-match-face-1 ((t (:inherit font-lock-doc-face :foreground nil))))
 '(lsp-ui-doc-background ((t (:background nil))))
 '(org-mode-line-clock ((t (:background nil :foreground "red" :box (:line-width -1 :style released-button)))) t))
