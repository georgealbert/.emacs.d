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

(require 'benchmark-init)

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

;; (advice-add #'package-initialize :after #'update-load-path)
;; (advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;; (setq comp-deferred-compilation t)

(require 'init-core)
(require 'init-editor)

(require 'init-ivy)

(require 'init-keybinds)
(require 'init-lang)
(require 'init-tools)
(require 'init-org)
;; (require 'init-keyfreq)
(require 'init-editor-paren)
(require 'init-hydra)
(require 'init-ui)
(require 'init-ui-theme)
(require 'init-tree-sitter)

(require 'init-lsp-bridge)
(require 'init-vcs)
;; (require 'init-erc)

;; maximize window
(if (or (eq system-type 'windows-nt) (eq system-type 'darwin) (eq window-system 'x))
    (toggle-frame-maximized))

(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file 'no-error 'no-message)
