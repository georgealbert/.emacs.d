;;; init-completion.el -*- lexical-binding: t; -*-

(use-package helm
  :diminish
  :config
  (helm-mode +1)
  :init
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          ;;
          helm-candidate-number-limit 100
          ;; this actually updates things reeeelatively quickly.
          helm-input-idle-delay 0.01
          ;; Remove extraineous helm UI elements
          helm-display-header-line nil
          ;; Default helm window sizes, 太小了，看着不舒服，不设为nil了。
          ;; helm-display-buffer-default-width nil
          ;; helm-display-buffer-default-height 0.25
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t
          helm-ff-file-name-history-use-recentf t
          ;; 在mini buffer中显示文件名长一点
          helm-buffer-max-length 40
          )
  :bind (("C-c n" . helm-mini)
         ;; ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ;; ("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-h a" . helm-apropos)
         ("C-c b" . helm-bookmarks)
         ("C-x c SPC" . helm-all-mark-rings)
         ("C-c C-r" . helm-resume)
         (:map minibuffer-local-map
               ("C-r" . helm-minibuffer-history)
               ("C-c C-l" . helm-minibuffer-history))
         :map helm-map
         ("<tab>" . 'helm-execute-persistent-action)
         ("C-i" . 'helm-execute-persistent-action)
         ("C-z" . 'helm-select-action)))

;; Turn off ido mode in case I enabled it accidentally
(ido-mode -1)

(use-package helm-swoop
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all)))

;; desc: 用ag搜索当前目录或者project root
;; homepage: https://github.com/syohex/emacs-helm-ag
(use-package helm-ag
  :bind
  (("s-f" . helm-ag)
   ("s-r" . helm-ag-project-root)))

;; helm可以使用amx作为后端了，branch: helm-amx
;; https://github.com/DarwinAwardWinner/amx/issues/23
(use-package amx
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/extensions/amx"
  :bind ("M-x" . amx)
  :init
  (setq amx-history-length 50)
  (setq amx-backend 'helm))

;; (use-package snails
;;   :ensure nil
;;   :load-path "~/.emacs.d/site-lisp/extensions/snails"
;;   ;; :bind ("s-e". '(snails '(snails-backend-buffer snails-backend-recentf) t))
;;   :config
;;   (global-set-key (kbd "s-e") '(lambda ()
;;                                  (interactive)
;;                                  (snails '(snails-backend-buffer snails-backend-recentf) t)))
;;   )

(provide 'init-completion)
