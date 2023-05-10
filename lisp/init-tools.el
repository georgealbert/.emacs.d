;;; init-tools.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; tools/flycheck/autoload.el
(defun +flycheck|init-popups ()
  "Activate `flycheck-posframe-mode' if available and in GUI Emacs.
Activate `flycheck-popup-tip-mode' otherwise.
Do nothing if `lsp-ui-mode' is active and `lsp-ui-sideline-enable' is non-nil."
  (unless (and (bound-and-true-p lsp-ui-mode)
               lsp-ui-sideline-enable)
    (if (and (fboundp 'flycheck-posframe-mode)
             (display-graphic-p))
        (flycheck-posframe-mode +1)
      (flycheck-popup-tip-mode +1))))

(use-package flycheck-posframe
  ;; :when (and EMACS26+ (featurep! +childframe))
  :defer t
  :init (add-hook 'flycheck-mode-hook #'+flycheck|init-popups)
  :config
  (setq flycheck-posframe-warning-prefix "☎ "
        flycheck-posframe-info-prefix "··· "
        flycheck-posframe-error-prefix "✕ "))

;; disable default vc
(setq vc-handled-backends nil)

(use-package magit
  :mode (("\\COMMIT_EDITMSG\\'" . text-mode)
         ("\\MERGE_MSG\\'" . text-mode))
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-popup))
  :config
  (progn
    (setq magit-last-seen-setup-instructions "1.4.0")))

(use-package ggtags
  :ensure t
  :defer t)

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (progn
                ;; (flycheck-mode 1)
                (ggtags-mode 1)))))

;; (use-package undo-tree
;;   :disabled t
;;   :defer 1
;;   :diminish
;;   :config
;;   (progn
;;     (global-undo-tree-mode)
;;     (setq undo-tree-visualizer-timestamps t)
;;     (setq undo-tree-visualizer-diff t)))

(use-package vundo
  :defer 2
  :config
  ;; (setq vundo-glyph-alist vundo-unicode-symbols)
  (set-face-attribute 'vundo-default nil :family "Symbola"))

(use-package aweshell
  :load-path "~/.emacs.d/site-lisp/extensions/aweshell"
  :disabled t
  :bind (("s-n" . aweshell-new)
         ("s-h" . aweshell-toggle)
         ("s-x s-x" . aweshell-dedicated-toggle)))

(defun my-overview-of-current-buffer ()
  "toggle overview,  @see http://emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/"
  (interactive)
  (set-selective-display (if selective-display nil 1)))

;; homepage: https://github.com/technomancy/find-file-in-project
(use-package find-file-in-project
  :defer t
  :config
  (when IS-WINDOWS
    (setq ffip-find-executable "c:/msys64/usr/bin/find")))

(use-package pyim
  :disabled t
  :load-path "~/.emacs.d/site-lisp/extensions/pyim"
  :config
  ;; (use-package pyim-basedict
  ;;   :ensure nil
  ;;   :config (pyim-basedict-enable))

  ;; homepage: https://github.com/merrickluo/liberime
  (use-package liberime
    :load-path "~/.emacs.d/site-lisp/extensions/liberime"
    ;;  :init
    ;;  ;; 不能和小狼毫用同一个目录进行配置，否则会互相覆盖。 
    ;;  ;; (setq liberime-user-data-dir "~/weasel")

    ;;  (add-hook 'after-liberime-load-hook
    ;;            (lambda ()
    ;;              (run-with-timer
    ;;               5 1
    ;;               (message "use luna_pinyin_simp")
    ;;               (liberime-select-schema "luna_pinyin_simp"))))
    :config
    (liberime-select-schema "luna_pinyin_simp"))

  (setq pyim-default-scheme 'rime-quanpin)

  (setq pyim-dcache-backend 'pyim-sqlite)
  (use-package sqlite3
    :load-path "e:/workspace/emacs-sqlite3")

  (progn
    (setq default-input-method "pyim")

    ;; (setq pyim-default-scheme 'quanpin)
    ;; 应该是不能用vc编译的librime.dll，必须用mingw64编译。

    (setq-default pyim-english-input-switch-functions
                  '(pyim-probe-dynamic-english
                    ;; pyim-probe-isearch-mode
                    pyim-probe-program-mode
                    pyim-probe-org-structure-template))

    (setq-default pyim-punctuation-half-width-functions
                  '(pyim-probe-punctuation-line-beginning
                    pyim-probe-punctuation-after-punctuation))

    ;; 不用isearch，用的是swiper。
    ;; (pyim-isearch-mode 1)

    (setq pyim-page-tooltip 'posframe)

    (setq pyim-page-length 9)

    (setq pyim-fuzzy-pinyin-alist
          '(("eng" "en")
            ("ing" "in")
            ("ch" "c")
            ("sh" "s")
            ("zh" "z")))
    ;; 使用thread，不用subprocess 的方式
    (setq pyim-prefer-emacs-thread t)
    )
  :bind
  (("M-j" . pyim-convert-string-at-point)
   ;; ("C-;" . pyim-delete-word-from-personal-buffer)
   ;; ("," . pyim-page-previous-page)
   ;; ("." . pyim-page-next-page)
   ))

;; 自己写的就是爽啊，没办法就是爽
(if (fboundp 'w32-set-ime-open-status)
    (progn
      (defun emacs-ime-disable ()
        (w32-set-ime-open-status nil))

      (defun emacs-ime-enable ()
        (w32-set-ime-open-status t))

      (add-hook 'evil-insert-state-entry-hook 'emacs-ime-enable)
      (add-hook 'evil-insert-state-exit-hook 'emacs-ime-disable)
      ))

(if (and (eq system-type 'darwin) (fboundp 'mac-input-source))
    (progn
      ;; (defvar asc-ime (mac-input-source))
      (defvar last-ime (mac-input-source))
      (defun emacs-ime-disable ()
        ;; (start-process "set-input-source" nil "/usr/local/bin/macism" "com.apple.keylayout.ABC"))
        (setq last-ime (mac-input-source))
        (mac-select-input-source "com.apple.keylayout.ABC")
        )

      (defun emacs-ime-enable ()
        ;; (start-process "set-input-source" nil "/usr/local/bin/macism" "im.rime.inputmethod.Squirrel.Rime"))
        ;; (mac-select-input-source "im.rime.inputmethod.Squirrel.Rime")
        (mac-select-input-source last-ime)
        )

      (add-hook 'evil-insert-state-entry-hook 'emacs-ime-enable)
      (add-hook 'evil-insert-state-exit-hook 'emacs-ime-disable)
      ))


;; https://www.albertzhou.net/blog/2020/03/emacs-sdcv.html
(use-package sdcv
  :defer t
  :load-path "~/.emacs.d/site-lisp/extensions/sdcv"
  :bind (("s-t p" . sdcv-search-pointer)  ;; 光标处的单词, buffer显示
         ("s-t t" . sdcv-search-pointer+) ;; 光标处的单词, frame显示
         ("s-t i" . sdcv-search-input)    ;; 输入的单词, buffer显示
         ("s-t ;" . sdcv-search-input+))
  :init
  ;; fix non-prefix "s-t" key error.
  (define-key global-map (kbd "s-t") (make-sparse-keymap))
  ;; (global-set-key (kbd "s-t p") 'sdcv-search-pointer)

  :config
  (when IS-WINDOWS
    (progn
      (setq sdcv-program "D:\\emacs\\bin\\sdcv.exe")
      ;; set local sdcv dict to search word
      (setq sdcv-dictionary-data-dir "c:\\home\\albert\\stardict")))

  (when (eq system-type 'darwin)
    (progn
      (setq sdcv-dictionary-data-dir "/Users/albert/stardict")))

  (setq sdcv-dictionary-simple-list        ;; a simple dictionary list
        '(;; "懒虫简明汉英词典"
          ;; "KDic11万英汉词典"
          "新华字典"
          "牛津现代英汉双解词典"
          "朗道英汉字典5.0"
          "朗道汉英字典5.0"
          "懒虫简明英汉词典"))

  ;; (shell-command-to-string "E:\\emacs\\bin\\sdcv.exe -n --data-dir=\"e:\\home\\albert\\stardict\" \"test\"")
  ;; (shell-command-to-string "env LANG=zh_CN.UTF-8 sdcv -n --data-dir ~/stardict test")
  (setq sdcv-tooltip-timeout 0))

;;
;; https://emacs-china.org/t/topic/5507
;; 1. 导出 PATH 到 ~/.path
;; /bin/zsh -c -l -i 'printf "%s" "$PATH"' > ~/.path
;;
;; 2. 给 Emacs 设置 PATH 和 exec-path
;; (getenv "PATH")
(when IS-MAC
  (condition-case err
      (let ((path (with-temp-buffer
                    (insert-file-contents-literally "~/.path")
                    (buffer-string))))
        (setenv "PATH" path)
        ;; for pyenv
        (setenv "PYENV_ROOT" "/usr/local/var/pyenv")
        (setq exec-path (append (parse-colon-path path) (list exec-directory))))
    (error (warn "%s" (error-message-string err)))))

;; helm可以使用amx作为后端了，branch: helm-amx
;; https://github.com/DarwinAwardWinner/amx/issues/23
(use-package amx
  :ensure nil
  :defer t
  :load-path "~/.emacs.d/site-lisp/extensions/amx"
  ;; :bind ("M-x" . amx)
  :init
  (setq amx-history-length 50))


(use-package color-rg
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/extensions/color-rg"
  :bind (("s-x g" . color-rg-search-symbol)
         ("s-x h" . color-rg-search-input)
         ("s-x j" . color-rg-search-symbol-in-project)
         ("s-x r" . color-rg-search-input-in-project)
         ("s-x ," . color-rg-search-symbol-in-current-file)
         ("s-x f" . color-rg-search-input-in-current-file))
  :init
  (setq color-rg-mac-load-path-from-shell nil)
  (define-key global-map (kbd "s-x") (make-sparse-keymap))
)


;; symbol-overlay
;; homepage: https://github.com/wolray/symbol-overlay
;;
;; 作者的知乎，新插件推荐，高亮symbol同时支持一键跳转 https://zhuanlan.zhihu.com/p/26471685
;; 同时高亮多个symbol https://emacs-china.org/t/package-symbol-overlay-symbol/7706
;; 
;; 老王的使用中提到了 https://manateelazycat.github.io/emacs/2022/11/07/how-i-use-emacs.html
;; 用 Emacs 的都少不了 isearch, 但是 isearch 不方便的地方是每次都要手动输入或者 yank 当前 symbol 给 isearch， 同时要批量替换的按键流程也很繁琐。 在使用 symbol-overlay 之前我一直用我自己开发的 lazy-search, 这两个项目的目标都是启动后立即选中光标处的 symbol, 再按单按键比如按 n/p 后， 快速跳转上一个和下一个匹配项， 节省了大量选中当前 symbol 启动 isearch 再粘贴 symbol 的操作时间。 用了 symbol-overlay 后， 发现比我的 lazy-search 实现的更加简洁和强大， 包括搜索后快速按 r 键可以对所有匹配的 symbol 进行快速重命名操作， symbol-overlay 基本上是单文件重构场景下最好用的插件， 强烈推荐大家使用。
;;
(use-package symbol-overlay
  :defer 2
  :config
  (global-set-key (kbd "M-i") 'symbol-overlay-put)
  (global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
  (global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
  (global-set-key (kbd "<f7>") 'symbol-overlay-mode)
  (global-set-key (kbd "<f8>") 'symbol-overlay-remove-all)
  )


(provide 'init-tools)
