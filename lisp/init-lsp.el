;;; init-core.el -*- lexical-binding: t; -*-

;; [2020-02-03 周一 22:22:16]
;; emacs 字符串连接对数据越大越慢，lsp的completion一次接收大量的数据需要进行字符串连接，
;; emacs的process-filter 一次默认只传输4096个字节，如果数据量
;; 过大就要对数据合并之后再进行json decode 这就会慢到爆炸。
;; 从emacs27开始，可以修改read-process-output-max
(setq read-process-output-max (* 1024 1024))

;; Emacs client for the Language Server Protocol
;; https://github.com/emacs-lsp/lsp-mode#supported-languages
(use-package lsp-mode
  ;; :diminish lsp-mode
  :defer t
  ;; :hook (prog-mode . lsp)
  :hook (python-mode . lsp-deferred)
        ;; (go-mode . lsp-deferred)
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point))
  :init
  (setq lsp-auto-guess-root nil)       ; Detect project root 貌似设置为t后，go总是找不到对应的package
  ; Auto-kill LSP server
  (setq lsp-keep-workspace-alive nil)
  ;; disable Yasnippet
  (setq lsp-enable-snippet nil)
  (setq lsp-prefer-flymake nil)      ; Use lsp-ui and flycheck
  (setq flymake-fringe-indicator-position 'right-fringe)

  ;; enable log only for debug
  (setq lsp-log-io nil)
  
  ;; use `evil-matchit' instead
  (setq lsp-enable-folding nil)
  
  ;; use `company-ctags' only.
  ;; Please note `company-lsp' is automatically enabled if installed
  ;; (setq lsp-enable-completion-at-point nil)

  ;; turn off for better performance
  (setq lsp-enable-symbol-highlighting nil)

  ;; use ffip instead
  (setq lsp-enable-links nil)
)

(use-package lsp-ui
  :defer t
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :init (setq lsp-ui-doc-enable t
              lsp-ui-doc-use-webkit nil
              lsp-ui-doc-include-signature t
              lsp-ui-doc-position 'top
              lsp-ui-doc-border (face-foreground 'default)

              ;; lsp-enable-snippet nil
              lsp-ui-sideline-enable nil

              ;; emacs26.2 经常陷入卡顿, set it to nil.
              ;; lsp-use-native-json nil

              ;; emacs 27.0.50, https://emacs-lsp.github.io/lsp-mode/lsp-mode.html
              lsp-use-native-json t

              lsp-json-use-lists t

              lsp-ui-sideline-ignore-duplicate t)
  :config
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))

(use-package company-lsp
  :defer t
  :init (setq company-lsp-cache-candidates 'auto))
  
(use-package lsp-treemacs
  :defer t
  :bind (:map lsp-mode-map
  ("M-9" . lsp-treemacs-errors-list)))

(use-package lsp-python-ms
  ;; :ensure nil
  ;; :defer t
  :demand
  :after lsp-mode
  :hook (python-mode . lsp)
  :config
  ;; :init
  ;; for dev build of language server
  (setq lsp-python-ms-dir
        (expand-file-name "e:/workspace/python-language-server/output/bin/Release/"))
  ;; for executable of language server, if it's not symlinked on your PATH
  (setq lsp-python-ms-executable
        "e:/workspace/python-language-server/output/bin/Release/Microsoft.Python.LanguageServer.exe"))


(provide 'init-lsp)