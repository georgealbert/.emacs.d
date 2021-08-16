;;; init-completion.el -*- lexical-binding: t; -*-

(use-package helm
  :diminish
  :disabled t
  :config
  (helm-mode +1)
  :init
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 2.0 ; update fast sources immediately (doesn't). 0.0 -> 2.0
          ;;
          helm-candidate-number-limit 50
          ;; this actually updates things reeeelatively quickly.
          ;; 0.01 -> 0.1
          ;; helm-input-idle-delay 0.1
          ;; Remove extraineous helm UI elements
          helm-display-header-line nil
          ;; Default helm window sizes, 太小了，看着不舒服，不设为nil了。
          ;; helm-display-buffer-default-width nil
          ;; helm-display-buffer-default-height 0.25
          helm-yas-display-key-on-candidate t
          ;; cpu usage is high [2020-12-09 Wed 21:20:42]
          ;; helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t
          helm-ff-file-name-history-use-recentf t
          ;; 在mini buffer中显示文件名长一点
          helm-buffer-max-length 40
          )
  :bind (("C-c n" . helm-mini)
         ;; ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
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
  :disabled t
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all)
         ;; Move up and down like isearch
         (:map helm-swoop-map
               ("C-r" . 'helm-previous-line)
               ("C-s" . 'helm-next-line))
         (:map helm-multi-swoop-map
               ("C-r" . 'helm-previous-line)
               ("C-s" . 'helm-next-line)))
  :config
  ;; Go to the opposite side of line from the end or beginning of line
  (setq helm-swoop-move-to-line-cycle t))

;; desc: 用ag搜索当前目录或者project root
;; homepage: https://github.com/syohex/emacs-helm-ag
(use-package helm-ag
  :disabled t
  :bind
  (("s-f" . helm-ag)
   ("s-r" . helm-ag-project-root)))

(provide 'init-completion)
