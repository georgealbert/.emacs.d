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

(defun albert/datastruct-member (tag)
  "parse tag generated by semantic-parse-region."

  (defun fp-str (str ftag)
    "function pointer args to str. 函数指针参数的处理.如ngx_tcp_module_t里的函数指针"
    (cond
     ((null ftag) "")
     ((cond
       ((semantic-tag-p ftag)
        (let* ((name (semantic-tag-name ftag))
               (type (semantic-tag-get-attribute ftag :type))
               (pointer (semantic-tag-get-attribute ftag :pointer)))
          (cond
           ((atom type) (setq str (concat str (format "%s " type))))
           ((semantic-tag-p type) 
            (setq str (concat str (format "%s " (semantic-tag-name type))))))
          
          (if (numberp pointer)
              (setq str (concat str (make-string pointer ?*))))

          (setq str (concat str (format "%s, " name)))
          ))
       (t (setq str (concat (fp-str str (car ftag)) (fp-str str (cdr ftag)))))
       ))
     ))

  (defun var-str (str index vtag)
    (setq str (format "    <f%d>" index))
    (cond
     ((null vtag) "")
     ((cond
       ((semantic-tag-p vtag)
        (let* ((name (semantic-tag-name vtag))
               (type (semantic-tag-get-attribute vtag :type))
               ;;(function-pointer (semantic-tag-get-attribute vtag :function-pointer))
               (pointer (semantic-tag-get-attribute vtag :pointer)))
          (cond
           ((atom type) (setq str (concat str (format "%s " type))))
           ;; 函数指针要单独处理,如ngx_tcp_module_t里的函数指针
           ((semantic-tag-p type) 
            (setq str (concat str (format "%s " (semantic-tag-name type))))))
          
          (if (numberp pointer)
              (setq str (concat str (make-string pointer ?*))))

          (if (semantic-tag-get-attribute vtag :function-pointer)
              (let* ((tmp (fp-str "" (semantic-tag-components vtag)))
                     (args (substring tmp 0 (- (length tmp) 2))))
                (setq str (format "%s(*%s)(%s) \\l|\\\n" str name args)))
            (concat str (format "%s \\l|\\\n" name)))
          ))
       (t (concat (var-str str (+ index 1) (car vtag)) (var-str str (+ index 1) (cdr vtag))))
       ))
     ))
  (var-str "" 0 tag))

(defun albert/datastruct-dot-head (tag)
  "datastruct name to dot head."
  (let ((name (semantic-tag-name tag)))
    (format "subgraph cluster_%s {
  node [shape=record fontsize=12 fontname=Courier style=filled];
  color=lightgray;
  style=filled;
  label = \"Struct %s\";
  edge[color=\"#2e3436\"];
  node_%s [shape=record label=\"\n" name name name)))

(defun albert/datastruct-dot-end (str)
  "datastruct to dot end string. 去掉最后一个字段的\l|\n"
  (concat (substring str 0 (- (length str) 3)) "\"];
}\n"))

(defun albert/semantic-lex-buffer (start end)
  "parse c datastruct definition using semantic and insert it into `buffer'"
  (interactive "rp")
  (message "start=%d, end=%d" start end)
  (let* ((tag (car (semantic-parse-region start end)))
         (members (plist-get (semantic-tag-class (semantic-tag-get-attribute tag :typedef)) :members)))
    ;; (message "%s\n" tag)
    ;;(message "%s" (albert/datastruct-dot-head tag))
    ;; (message "%s" (albert/datastruct-member (semantic-tag-components tag)))
    (let* ((body (albert/datastruct-member (semantic-tag-components tag)))
           (dot_str (albert/datastruct-dot-end body)))
      ;;(message "%s" (albert/datastruct-dot-end body))
      (save-excursion
        ;;(iter 0)
        (set-buffer (get-buffer-create "tmp.dot"))
        ;;(graphviz-dot-mode)
        (setq pos (point-max))
        (insert (albert/datastruct-dot-head tag) 
                dot_str)
        (goto-char (point-max))
        (delete-char -1)
        )

      (if (one-window-p)
          (split-window-vertically))

      (switch-to-buffer-other-window "tmp.dot")
      (goto-char (point-min))
      )))

;; (plist-get (nthcdr 2 (semantic-tag-get-attribute tag :typedef)) :members))

;; (message (semantic-parse-region start end 1)))
;; (message (semantic-c-lexer start end 1)))
;; (message (semantic-lex-buffer)))
;;(semantic-lex start end 1))

;; disable default vc
(setq vc-handled-backends nil)

(use-package magit
  ;; :load-path "~/elisp/magit/lisp"
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

(use-package undo-tree
  :ensure t
  :defer 1
  ;; :diminish undo-tree-mode
  :diminish
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(use-package aweshell
  :load-path "~/.emacs.d/site-lisp/extensions/aweshell"
  :disabled t
  :bind (("s-n" . aweshell-new)
         ("s-h" . aweshell-toggle)
         ("s-x s-x" . aweshell-dedicated-toggle)))

(defun my-overview-of-current-buffer ()
  "toggle overview,  @see http://emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/
"
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
   )
  )

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

;; [2020-12-02 三 16:38:53] 折腾了半天，也改了代码，发现不稳定，一切换到rime，输入字符，就把emacs卡死了，还是
;; 用外部输入法切换吧，比较稳定。舒服了
(if (eq system-type 'darwin)
    (progn
      (defun emacs-ime-disable ()
        (start-process "set-input-source" nil "/usr/local/bin/macism" "com.apple.keylayout.ABC"))
      ;; (mac-select-input-source "com.apple.keylayout.ABC"))

      (defun emacs-ime-enable ()
        (start-process "set-input-source" nil "/usr/local/bin/macism" "im.rime.inputmethod.Squirrel.Rime"))
      ;; (mac-select-input-source "im.rime.inputmethod.Squirrel.Rime"))

      (add-hook 'evil-insert-state-entry-hook 'emacs-ime-enable)
      (add-hook 'evil-insert-state-exit-hook 'emacs-ime-disable)
      ))


;; https://www.albertzhou.net/blog/2020/03/emacs-sdcv.html
(use-package sdcv
  :defer t
  :disabled t
  :load-path "~/.emacs.d/site-lisp/extensions/sdcv"
  :bind (("s-t p" . sdcv-search-pointer)  ;; 光标处的单词, buffer显示
         ("s-t t" . sdcv-search-pointer+) ;; 光标处的单词, frame显示
         ("s-t i" . sdcv-search-input)    ;; 输入的单词, buffer显示
         ("s-t ;" . sdcv-search-input+))
  :config
  (setq sdcv-program "E:\\emacs\\bin\\sdcv.exe")

  (setq sdcv-dictionary-data-dir "e:\\home\\albert\\stardict")   ;; set local sdcv dict to search word

  (setq sdcv-dictionary-simple-list        ;; a simple dictionary list
        '(
          "牛津现代英汉双解词典"
          "朗道英汉字典5.0"
          "懒虫简明英汉词典"
          ;; "懒虫简明汉英词典"
          ;; "KDic11万英汉词典"
          ))

  (setq sdcv-tooltip-timeout 0)

  ;; (shell-command-to-string "E:\\emacs\\bin\\sdcv.exe -n --data-dir=\"e:\\home\\albert\\stardict\" \"test\"")
  )

;; [2020-11-23 Mon 00:00:33]
(use-package exec-path-from-shell
  :if (memq window-system '(ns mac))
  :ensure t
  :config
  ;; (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(use-package sis
  ;; :hook
  ;; enable the /follow context/ and /inline region/ mode for specific buffers
  ;; (((text-mode prog-mode) . sis-context-mode)
  ;;  ((text-mode prog-mode) . sis-inline-mode))

  :disabled t
  :config
  ;; For MacOS
  (sis-ism-lazyman-config
  
   ;; English input source may be: "ABC", "US" or another one.
   ;; "com.apple.keylayout.ABC"
   "com.apple.keylayout.US"
   ;; "im.rime.inputmethod.Squirrel.Rime"

   ;; Other language input source: "rime", "sogou" or another one.
   "im.rime.inputmethod.Squirrel.Rime"
   ;;"com.sogou.inputmethod.sogou.pinyin"
   )

  ;; enable the /cursor color/ mode
  (sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)
  ;; enable the /context/ mode for all buffers
  (sis-global-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  (sis-global-inline-mode t)
  )

(provide 'init-tools)
