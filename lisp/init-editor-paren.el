;;; init-editor-paren.el -*- lexical-binding: t; -*-

;;;###package paren
;; 显示匹配的括号

;; [2020-02-16 周日 14:13:27] 不知道为什么用use-package后，在右括号处匹配左括号不正常。
;; emacs自带的paren.el
(use-package paren
  :ensure nil
  :defer 2
  ;; :hook (prog-mode . show-paren-mode)
  ;; :config
  ;; (setq show-paren-delay 0.1
  ;;       show-paren-highlight-openparen t
  ;;       show-paren-when-point-inside-paren nil
  ;;       show-paren-when-point-in-periphery nil)
  :init
  (show-paren-mode +1)
  )

;;
;; [2020-02-07 周五 12:17:50] 感觉smartparens太复杂了，改用awesome-pair
;;
;; (use-package smartparens
;;   ;; Auto-close delimiters and blocks as you type. It's more powerful than that,
;;   ;; but that is all Doom uses it for.
;;   ;;:after-call doom-switch-buffer-hook after-find-file
;;   ;; :hook (after-init . show-paren-mode)
;;   :disabled t
;;   :hook (prog-mode . show-smartparens-mode)
;;   ;; :defer 2
;;   :diminish smartparens-mode
;;   :commands sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string
;;   :config
;;   ;; Load default smartparens rules for various languages
;;   (require 'smartparens-config)

;;   ;; Overlays are too distracting and not terribly helpful. show-parens does
;;   ;; this for us already (and is faster), so...
;;   (setq sp-highlight-pair-overlay nil
;;         sp-highlight-wrap-overlay nil
;;         sp-highlight-wrap-tag-overlay nil)
;;   (with-eval-after-load 'evil
;;     ;; But if someone does want overlays enabled, evil users will be stricken
;;     ;; with an off-by-one issue where smartparens assumes you're outside the
;;     ;; pair when you're really at the last character in insert mode. We must
;;     ;; correct this vile injustice.
;;     (setq sp-show-pair-from-inside t)
;;     ;; ...and stay highlighted until we've truly escaped the pair!
;;     (setq sp-cancel-autoskip-on-backward-movement nil))

;;   ;; The default is 100, because smartparen's scans are relatively expensive
;;   ;; (especially with large pair lists for somoe modes), we halve it, as a
;;   ;; better compromise between performance and accuracy.
;;   (setq sp-max-prefix-length 50)
;;   ;; This speeds up smartparens. No pair has any business being longer than 4
;;   ;; characters; if they must, set it buffer-locally.
;;   (setq sp-max-pair-length 4)
;;   ;; This isn't always smart enough to determine when we're in a string or not.
;;   ;; See https://github.com/Fuco1/smartparens/issues/783.
;;   (setq sp-escape-quotes-after-insert nil)

;;   ;; Silence some harmless but annoying echo-area spam
;;   (dolist (key '(:unmatched-expression :no-matching-tag))
;;     (setf (alist-get key sp-message-alist) nil))

;;   (add-hook 'minibuffer-setup-hook
;;     (defun doom-init-smartparens-in-minibuffer-maybe-h ()
;;       "Enable `smartparens-mode' in the minibuffer, during `eval-expression' or
;; `evil-ex'."
;;       (when (memq this-command '(eval-expression evil-ex))
;;         (smartparens-mode))))

;;   ;; You're likely writing lisp in the minibuffer, therefore, disable these
;;   ;; quote pairs, which lisps doesn't use for strings:
;;   (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
;;   (sp-local-pair 'minibuffer-inactive-mode "`" nil :actions nil)

;;   ;; Smartparens breaks evil-mode's replace state
;;   (defvar doom-buffer-smartparens-mode nil)
;;   (add-hook 'evil-replace-state-exit-hook
;;     (defun doom-enable-smartparens-mode-maybe-h ()
;;       (when doom-buffer-smartparens-mode
;;         (turn-on-smartparens-mode)
;;         (kill-local-variable 'doom-buffer-smartparens-mode))))
;;   (add-hook 'evil-replace-state-entry-hook
;;     (defun doom-disable-smartparens-mode-maybe-h ()
;;       (when smartparens-mode
;;         (setq-local doom-buffer-smartparens-mode t)
;;         (turn-off-smartparens-mode))))
;;   ;; (show-smartparens-global-mode +1)
;;   (smartparens-global-mode +1))

;; http://ergoemacs.org/emacs/emacs_editing_lisp.html
;; url: http://www.emacswiki.org/emacs/HighlightParentheses
;; usage: 用不同颜色高亮显示每一级括号
(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode)
  :diminish
  :config
  (setq hl-paren-colors
    ;; 从左到右，依次是从内到外的括号的颜色
    ;; '("firebrick1" "green" "purple" "DeepPink" "DeepSkyBlue" "violet" "IndianRed1" "IndianRed3" "IndianRed4")
    '("firebrick1" "green" "purple" "DeepPink" "DeepSkyBlue" "violet")
    ))

(provide 'init-editor-paren)
