;;; init-completion.el -*- lexical-binding: t; -*-

(use-package helm
  :ensure t
  :defer t
  :diminish helm-mode
  :config
  (helm-mode +1)
  
  ;;  (defun spacemacs//helm-hide-minibuffer-maybe ()
  ;;    "Hide minibuffer in Helm session if we use the header line as input field."
  ;;    (when (with-helm-buffer helm-echo-input-in-header-line)
  ;;      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
  ;;        (overlay-put ov 'window (selected-window))
  ;;        (overlay-put ov 'face
  ;;                     (let ((bg-color (face-background 'default nil)))
  ;;                       `(:background ,bg-color :foreground ,bg-color)))
  ;;        (setq-local cursor-type nil))))

  ;;  (add-hook 'helm-minibuffer-set-up-hook
  ;;          'spacemacs//helm-hide-minibuffer-maybe)
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
          ;;
          helm-yas-display-key-on-candidate t
          ;;
          helm-quick-update t
          ;;
          helm-M-x-requires-pattern nil
          ;;
          helm-ff-skip-boring-files t
          ;; open helm buffer inside current window, not occupy whole other window
          ;; 不习惯
          ;; helm-split-window-in-side-p t
          ;; move to end or beginning of source when reaching top or bottom of source.
          ;; 不习惯，按C-n的时候循环就到不了其他的region了。
          ;; helm-move-to-line-cycle-in-source t
          helm-ff-file-name-history-use-recentf t
          ;;helm-echo-input-in-header-line t
          ;; 在mini buffer中显示文件名长一点
          helm-buffer-max-length 40
          )
  :bind (;; ("C-c m" . helm-mini)
         ("C-c n" . helm-mini)
         ;; ("C-x C-b" . helm-buffers-list)
         ;; ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ;; ("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-h a" . helm-apropos)
         ("C-x c SPC" . helm-all-mark-rings)
         :map helm-map
         ("<tab>" . 'helm-execute-persistent-action)
         ("C-i" . 'helm-execute-persistent-action)
         ("C-z" . 'helm-select-action)))

;; Turn off ido mode in case I enabled it accidentally
(ido-mode -1)

(use-package helm-swoop
  ;; :ensure t
  :defer t
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all)))

;; https://github.com/syohex/emacs-helm-ag
(use-package helm-ag
  :bind
  (("s-f" . helm-ag)
   ;; ("s-s f" . helm-ag)
   ;; ("s-s r" . helm-ag-project-root)
   ("s-r" . helm-ag-project-root)))

(provide 'init-completion)
