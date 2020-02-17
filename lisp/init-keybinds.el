;;; init-keybinds.el -*- lexical-binding: t; -*-

;; Display available keybindings in popup
(use-package which-key
  :diminish
  :bind (:map help-map ("C-h" . which-key-C-h-dispatch))
  :hook (after-init . which-key-mode)
  ;; :config
  ;; (setq which-key-allow-imprecise-window-fit t) ; performance 开了以后显示太小了，看着不舒服
  )

;; https://oremacs.com/2019/04/07/swiper-isearch/
;; https://pengpengxp.github.io/emacs/emacs_isearch_summary.html
(global-set-key (kbd "C-s") 'swiper-isearch)

(run-with-idle-timer
 1 nil
 #'(lambda ()
     (general-create-definer my-spc-leader-def
       :prefix "SPC"
       :states '(normal visual))

     (my-spc-leader-def
      "`" '(evil-switch-to-windows-last-buffer :which-key "Switch to last buffer")
      "." '(counsel-find-file :which-key "Find file")
      ";" '(counsel-M-x :which-key "counsel-M-x")
      "'" '(pp-eval-expression :which-key "Eval expression")
      ;; "cd" 'counsel-recent-directory

      "b"  '(:ignore t :which-key "buffer")
      "bb" '(counsel-ibuffer :which-key "Switch buffer")
      "bk" '(kill-current-buffer :which-key "Kill buffer")
      "br" '(revert-buffer :which-key "Revert buffer")
      "bo" 'my-overview-of-current-buffer ;; 移动会很卡
      "bm" '((lambda () (interactive) (counsel-evil-marks t)) :which-key "counsel-evil-marks")

      "e"  '(:ignore t :which-key "eshell")
      "ee" '(aweshell-dedicated-toggle :which-key "dedicated eshell")

      "f"  '(:ignore t :which-key "file")
      "ff" '(counsel-buffer-or-recentf :which-key "Recent files")
      "fr" '(counsel-buffer-or-recentf :which-key "Recent files")
      "fc" '(dired-jump :which-key "Open current dir") ;; open the dired from current file
      "fd" '(dired :which-key "dired") ;; open the dired from current file
      ;; "fs" 'scratch

      "g"  '(:ignore t :which-key "git")
      "gg" 'magit-status

      "h" '(:ignore t :which-key "help")
      "h'" 'describe-char
      "hC" 'describe-coding-system
      "hF" 'describe-face
      "ha" 'helm-apropos
      "hd" 'describe-function
      "he" 'view-echo-area-messages
      "hf" 'find-function
      "hk" 'describe-key
      "hr" 'helm-resume
      "hv" 'describe-variable

      "t"  '(:ignore t :which-key "toggle")
      "tn" '(display-line-numbers-mode :which-key "Line numbers")
      "tl" '(toggle-truncate-lines :which-key "truncate line")

      "s"  '(:ignore t :which-key "search")
      "sf" '(helm-ag :which-key "Search current directory")
      "ss" 'counsel-grep-or-swiper
      "sr" '(helm-ag-project-root  :which-key "Search project root")

      "w"  '(:ignore t :which-key "window")
      ;; "wh" 'evil-window-left
      ;; "wl" 'evil-window-right
      ;; "wk" 'evil-window-up
      ;; "wj" 'evil-window-down
      "wr" 'evil-window-rotate-upwards
      "wv" 'evil-window-vsplit
      "wu" 'winner-undo
      ;; "wr" 'winner-redo
      )))

(provide 'init-keybinds)
