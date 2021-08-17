;;; init-keybinds.el -*- lexical-binding: t; -*-

;; Display available keybindings in popup
(use-package which-key
  ;; :diminish
  ;; :bind (:map help-map ("C-h" . which-key-C-h-dispatch))
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
     (general-create-definer my-comma-leader-def
       :prefix ","
       :states '(normal visual))

     (my-comma-leader-def
       "m" '(evil-switch-to-windows-last-buffer :which-key "Switch to last buffer")
       "c"  '(:ignore t :which-key "code")
       "ci" '(evilnc-comment-or-uncomment-lines :which-key "comment lines")
       "cl" '(evilnc-quick-comment-or-uncomment-to-the-line :which-key "quick comment lines")
       "cc" '(evilnc-copy-and-comment-lines :which-key "copy comment lines")
       ;; "cp" 'my-evilnc-comment-or-uncomment-paragraphs
       "ct" '(evilnc-comment-or-uncomment-html-tag :which-key "comment html tag") ; evil-nerd-commenter v3.3.0 required


       "bf" 'beginning-of-defun
       "bu" 'backward-up-list
       ;; "bb" (lambda () (interactive) (switch-to-buffer nil)) ; to previous buffer
       "ef" 'end-of-defun
       ;; "sc" 'scratch

       "t"  '(:ignore t :which-key "tags")
       "tr" 'counsel-etags-recent-tag
       "tf" 'counsel-etags-find-tag

       ;; "yy" 'counsel-browse-kill-ring
       ;; "cf" 'counsel-grep ; grep current buffer
       ;; "gf" 'counsel-git ; find file
       ;; "gg" 'my-counsel-git-grep ; quickest grep should be easy to press
       ;; "gd" 'ffip-show-diff-by-description ;find-file-in-project 5.3.0+
       ;; "gl" 'my-git-log-trace-definition ; find history of a function or range
       ;; "sh" 'my-select-from-search-text-history

       "ir" 'ivy-resume
       )

     ;; [2020-03-28 周六 21:45:52] binding太多快捷键到evil的normal和visual mode中，在debug native-comp在visual block mode insert时的问题，从evil-insert-state的debug看，可能会慢。不用的快捷键都去掉。
     (general-create-definer my-spc-leader-def
       :prefix "SPC"
       :states '(normal visual))

     (my-spc-leader-def
      ;; "`" '(evil-switch-to-windows-last-buffer :which-key "Switch to last buffer")
      ;; "." '(counsel-find-file :which-key "Find file")
      ;; ";" '(counsel-M-x :which-key "counsel-M-x")
      "'" '(pp-eval-expression :which-key "Eval expression")
      ;; "cd" 'counsel-recent-directory

      "b"  '(:ignore t :which-key "buffer")
      ;; "bb" '(counsel-ibuffer :which-key "Switch buffer")
      "bb" '(counsel-switch-buffer :which-key "Switch buffer")
      "bk" '(kill-current-buffer :which-key "Kill buffer")
      "br" '(revert-buffer :which-key "Revert buffer")
      ;; "bo" 'my-overview-of-current-buffer ;; 移动会很卡
      ;; "bm" '((lambda () (interactive) (counsel-evil-marks +1)) :which-key "counsel-evil-marks")
      "bm" '(counsel-evil-marks :which-key "counsel-evil-marks")

      ;; "e"  '(:ignore t :which-key "eshell")
      ;; "ee" '(aweshell-dedicated-toggle :which-key "dedicated eshell")

      "f"  '(:ignore t :which-key "file")
      ;; "ff" '(counsel-recentf :which-key "Recent files")
      ;; "fr" '(counsel-buffer-or-recentf :which-key "Buffer or Recent files")
      ;; "fo" '(dired-jump :which-key "Open current dir") ;; open the dired from current file
      ;; "fd" '(dired :which-key "dired") ;; open the dired from current file
      "fp" '(find-file-in-project :which-key "ff in project")
      "ff" '(find-file-in-current-directory :which-key "ff in current dir")
      "fn" '(find-file-in-project-at-point :which-key "ff at point")
      "fs" '(find-file-in-project-by-selected :which-key "ff selected")
      "fn" '(find-file-with-similar-name :which-key "ff similar-name") ; ffip v5.3.1
      "fd" '(find-directory-in-project-by-selected :which-key "fdip selected")
      ;; "fs" 'scratch

      ;; "g"  '(:ignore t :which-key "git")
      ;; "gg" 'magit-status

      "h" '(:ignore t :which-key "help")
      "h'" 'describe-char
      "hC" 'describe-coding-system
      "hF" 'describe-face
      ;; "ha" 'helm-apropos
      ;; "hd" 'describe-function
      "he" 'view-echo-area-messages
      ;; "hf" 'find-function
      ;; "hk" 'describe-key
      "hr" 'helm-resume
      ;; "hv" 'describe-variable

      "t"  '(:ignore t :which-key "toggle")
      ;; "tn" '(display-line-numbers-mode :which-key "Line numbers")
      "tl" '(toggle-truncate-lines :which-key "truncate line")

      "s"  '(:ignore t :which-key "search")
      "si" 'evilmi-select-items
      ;; "sf" '(helm-ag :which-key "Search current directory")
      ;; "sc" 'counsel-grep-or-swiper
      "ss" 'helm-ag-this-file
      ;; "sr" '(helm-ag-project-root  :which-key "Search project root")

      "w"  '(:ignore t :which-key "window")
      ;; "wh" 'evil-window-left
      ;; "wl" 'evil-window-right
      ;; "wk" 'evil-window-up
      ;; "wj" 'evil-window-down
      ;; "wr" 'evil-window-rotate-upwards
      ;; "wv" 'evil-window-vsplit
      ;; "wu" 'winner-undo
      ;; "wr" 'winner-redo
      "ww" 'narrow-or-widen-dwim
      )))

(provide 'init-keybinds)
