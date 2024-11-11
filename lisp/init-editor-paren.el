;;; init-editor-paren.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;;###package paren
;; 显示匹配的括号

;; [2020-02-16 周日 14:13:27] 不知道为什么用use-package后，在右括号处匹配左括号不正常。
;; doc: emacs自带的paren.el
(use-package paren
  :ensure nil
  ;; :defer 2
  :hook (doom-first-input . show-paren-mode)
  ;; :hook (prog-mode . show-paren-mode)
  ;; :hook (after-init . show-paren-mode)
  ;; :config
  ;; (setq show-paren-delay 0.1
  ;;       show-paren-highlight-openparen t
  ;;       show-paren-when-point-inside-paren nil
  ;;       show-paren-when-point-in-periphery nil)
  :init
  (show-paren-mode +1)
  )

;; http://ergoemacs.org/emacs/emacs_editing_lisp.html
;; url: http://www.emacswiki.org/emacs/HighlightParentheses
;; usage: 用不同颜色高亮显示每一级括号
(use-package highlight-parentheses
  :defer t
  :hook (prog-mode . highlight-parentheses-mode)
  :diminish
  :config
  (setq hl-paren-colors
    ;; 从左到右，依次是从内到外的括号的颜色
    ;; '("firebrick1" "green" "purple" "DeepPink" "DeepSkyBlue" "violet" "IndianRed1" "IndianRed3" "IndianRed4")
    '("firebrick1" "green" "purple" "DeepPink" "DeepSkyBlue" "violet")
    ))

(provide 'init-editor-paren)
