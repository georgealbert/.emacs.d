;;; init-vcs.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; disable default vc
(setq vc-handled-backends nil)

;; c-c c-q 退出 magit-blame
(use-package magit
  :mode (("\\COMMIT_EDITMSG\\'" . text-mode)
         ("\\MERGE_MSG\\'" . text-mode))
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-popup))
  :config
  (progn
    (setq magit-last-seen-setup-instructions "1.4.0")
    ;; https://github.com/magit/magit/issues/4057
    (setq magit-blame-time-format "%F")
    (setq magit-blame-styles
          '((headings (heading-format . "%a (%C): %s - %h\n"))
            (margin
             ;; (margin-format "%a (%C): %s - %H")
             (margin-format "%C %a")
             (margin-width . 20)
             (margin-face . magit-blame-margin)
             (margin-body-face magit-blame-dimmed))
            ))))

(use-package vc-msg
  :defer t
  :config
  (with-no-warnings
    (with-eval-after-load 'hydra
      (defhydra vc-msg-show-hydra (:color blue)
        ("s" vc-msg-git-show-code "show")
        ("c" (vc-msg-git-copy-info :id) "copy hash")
        ("e" (vc-msg-git-copy-info :summary) "copy message")
        ("m" (let* ((info vc-msg-previous-commit-info))
               (funcall 'magit-find-file
                        (plist-get info :id)
                        (concat (vc-msg-sdk-git-rootdir)
                                (plist-get info :filename)))) "magit-find-file")
        ("q" vc-msg-close "quit")
        ))

    (setq vc-msg-force-vcs "git")
    (setq vc-msg-copy-id-to-kill-ring nil)

    (defun my-vc-msg-git-format (info)
      "Format the message for popup from INFO.
From git-messenger."
      (let* ((author (plist-get info :author)))
        (cond
         ((string-match-p "Not Committed Yet" author)
          "* Not Committed Yet*")
         (t
          (format "%s\n%s%s\n%s%s\n%s%s %s\n%s\n%s\n%s"
                  (propertize "* vc-msg *" 'face 'font-lock-comment-face)
                  (propertize "Commit: " 'face 'font-lock-keyword-face)
                  (propertize (vc-msg-sdk-short-id (plist-get info :id)) 'face 'font-lock-comment-face)
                  (propertize "Author: " 'face 'font-lock-keyword-face)
                  (propertize author 'face 'font-lock-string-face)
                  (propertize "Date  : " 'face 'font-lock-keyword-face)
                  (propertize (vc-msg-sdk-format-datetime (plist-get info :author-time)) 'face 'font-lock-string-face)
                  (plist-get info :author-tz)
                  (propertize (make-string 38 ?─) 'face 'font-lock-comment-face)
                  (plist-get info :summary)
                  (propertize "\nPress q to quit" 'face '(:inherit (font-lock-comment-face italic))))
          ))))

    (defun my-vc-msg-show ()
      "Show commit message of current line.
If Git is used and some text inside the line is selected,
the correct commit which submits the selected text is displayed."
      (interactive)
      (let* (finish
             (plugin (vc-msg-find-plugin))
             (current-file (funcall vc-msg-get-current-file-function)))
        (if plugin
            (let* ((hydra-hint-display-type 'message)
                   (executer (plist-get plugin :execute))
                   (formatter (plist-get plugin :format))
                   (commit-info (and current-file
                                     (funcall executer
                                              current-file
                                              (funcall vc-msg-get-line-num-function)
                                              (funcall vc-msg-get-version-function))))
                   message
                   )

              (cond
               ((and commit-info (listp commit-info))
                ;; the message to display
                (setq message (funcall formatter commit-info))

                (setq vc-msg-previous-commit-info commit-info)

                ;; copy the commit it/hash/changelist
                (when vc-msg-copy-id-to-kill-ring
                  (let* ((id (vc-msg-get-friendly-id plugin commit-info)))
                    (kill-new id)
                    (message "%s => kill-ring" id)))

                (vc-msg-show-hydra/body)
                (let* ((popuped-message (vc-msg-clean message)))
                  (cond ((and (fboundp 'posframe-workable-p) (posframe-workable-p))
                         (let ((buffer-name "*my-vc-msg-show*"))
                           (posframe-show buffer-name
                                          :string (concat (propertize "\n" 'face '(:height 0.3))
                                                          popuped-message
                                                          "\n"
                                                          (propertize "\n" 'face '(:height 0.3)))
                                          :left-fringe 8
                                          :right-fringe 8
                                          :max-width (round (* (frame-width) 0.62))
                                          :max-height (round (* (frame-height) 0.62))
                                          :internal-border-width 1
                                          :internal-border-color "#61AFEF" ; #ed98cc
                                          :background-color (face-background 'tooltip nil t))
                           (unwind-protect
                               (push (read-event) unread-command-events)
                             (posframe-hide buffer-name))))
                        ((and (fboundp 'pos-tip-show) (display-graphic-p))
                         (pos-tip-show popuped-message))
                        ((fboundp 'lv-message)
                         (lv-message popuped-message)
                         (unwind-protect
                             (push (read-event) unread-command-events)
                           (lv-delete-window)))
                        (t (message "%s" popuped-message))))

                (run-hook-with-args 'vc-msg-hook (vc-msg-detect-vcs-type) commit-info))

               ((stringp commit-info)
                ;; Failed. Show the reason.
                (kill-new commit-info)
                (message commit-info))

               (t
                ;; Failed for unknown reason
                (message "Shell command failed.")))
              ))))

    (advice-add #'vc-msg-close :override #'ignore)
    (advice-add #'vc-msg-git-format :override #'my-vc-msg-git-format)
    (advice-add #'vc-msg-show :override #'my-vc-msg-show)
    ))

;; https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-vcs.el
;; Walk through git revisions of a file
(use-package git-timemachine
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit success :foreground unspecified))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning :foreground unspecified))))
  :bind (:map vc-prefix-map
         ("t" . git-timemachine))
  :hook ((git-timemachine-mode . (lambda ()
                                   "Improve `git-timemachine' buffers."
                                   ;; Display different colors in mode-line
                                   (if (facep 'mode-line-active)
                                       (face-remap-add-relative 'mode-line-active 'custom-state)
                                     (face-remap-add-relative 'mode-line 'custom-state))

                                   ;; Highlight symbols in elisp
                                   (and (derived-mode-p 'emacs-lisp-mode)
                                        (fboundp 'highlight-defined-mode)
                                        (highlight-defined-mode t))

                                   ;; Display line numbers
                                   (and (derived-mode-p 'prog-mode 'yaml-mode)
                                        (fboundp 'display-line-numbers-mode)
                                        (display-line-numbers-mode t))))
         (before-revert . (lambda ()
                            (when (bound-and-true-p git-timemachine-mode)
                              (user-error "Cannot revert the timemachine buffer"))))))

;; Pop up last commit information of current line
(use-package git-messenger
  :bind (:map vc-prefix-map
         ("p" . git-messenger:popup-message)
         :map git-messenger-map
         ("m" . git-messenger:copy-message))
  :init (setq git-messenger:show-detail t
              git-messenger:use-magit-popup t)
  :config
  (with-no-warnings
    (with-eval-after-load 'hydra
      (defhydra git-messenger-hydra (:color blue)
        ("s" git-messenger:popup-show "show")
        ("c" git-messenger:copy-commit-id "copy hash")
        ("m" git-messenger:copy-message "copy message")
        ("," (catch 'git-messenger-loop (git-messenger:show-parent)) "go parent")
        ("q" git-messenger:popup-close "quit")))

    (defun my-git-messenger:format-detail (vcs commit-id author message)
      (if (eq vcs 'git)
          (let ((date (git-messenger:commit-date commit-id))
                (colon (propertize ":" 'face 'font-lock-comment-face)))
            (concat
             (format "%s%s %s \n%s%s %s\n%s  %s %s \n"
                     (propertize "Commit" 'face 'font-lock-keyword-face) colon
                     (propertize (substring commit-id 0 8) 'face 'font-lock-comment-face)
                     (propertize "Author" 'face 'font-lock-keyword-face) colon
                     (propertize author 'face 'font-lock-string-face)
                     (propertize "Date" 'face 'font-lock-keyword-face) colon
                     (propertize date 'face 'font-lock-string-face))
             (propertize (make-string 38 ?─) 'face 'font-lock-comment-face)
             message
             (propertize "\nPress q to quit" 'face '(:inherit (font-lock-comment-face italic)))))
        (git-messenger:format-detail vcs commit-id author message)))

    (defun my-git-messenger:popup-message ()
      "Popup message with `posframe', `pos-tip', `lv' or `message', and dispatch actions with `hydra'."
      (interactive)
      (let* ((hydra-hint-display-type 'message)
             (vcs (git-messenger:find-vcs))
             (file (buffer-file-name (buffer-base-buffer)))
             (line (line-number-at-pos))
             (commit-info (git-messenger:commit-info-at-line vcs file line))
             (commit-id (car commit-info))
             (author (cdr commit-info))
             (msg (git-messenger:commit-message vcs commit-id))
             (popuped-message (if (git-messenger:show-detail-p commit-id)
                                  (my-git-messenger:format-detail vcs commit-id author msg)
                                (cl-case vcs
                                  (git msg)
                                  (svn (if (string= commit-id "-")
                                           msg
                                         (git-messenger:svn-message msg)))
                                  (hg msg)))))
        (setq git-messenger:vcs vcs
              git-messenger:last-message msg
              git-messenger:last-commit-id commit-id)
        (run-hook-with-args 'git-messenger:before-popup-hook popuped-message)
        (git-messenger-hydra/body)
        (cond ((and (fboundp 'posframe-workable-p) (posframe-workable-p))
               (let ((buffer-name "*git-messenger*"))
                 (posframe-show buffer-name
                                :string (concat (propertize "\n" 'face '(:height 0.3))
                                                popuped-message
                                                "\n"
                                                (propertize "\n" 'face '(:height 0.3)))
                                :left-fringe 8
                                :right-fringe 8
                                :max-width (round (* (frame-width) 0.62))
                                :max-height (round (* (frame-height) 0.62))
                                :internal-border-width 1
                                ;; :internal-border-color (face-background 'posframe-border nil t)
                                :background-color (face-background 'tooltip nil t))
                 (unwind-protect
                     (push (read-event) unread-command-events)
                   (posframe-hide buffer-name))))
              ((and (fboundp 'pos-tip-show) (display-graphic-p))
               (pos-tip-show popuped-message))
              ((fboundp 'lv-message)
               (lv-message popuped-message)
               (unwind-protect
                   (push (read-event) unread-command-events)
                 (lv-delete-window)))
              (t (message "%s" popuped-message)))
        (run-hook-with-args 'git-messenger:after-popup-hook popuped-message)))

    (advice-add #'git-messenger:popup-close :override #'ignore)
    (advice-add #'git-messenger:popup-message :override #'my-git-messenger:popup-message)))

;; 需要设置 vc-handled-backends ，不然blamer显示不了
(use-package blamer
  :disabled t
  :bind (;;("s-n" . blamer-show-commit-info)
         ("C-c i" . blamer-show-posframe-commit-info))
  :defer t
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  (blamer-show-avatar-p nil)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    ;; :background nil
                    :height 140
                    :italic t)))
  :config
  (global-blamer-mode 1))


;; https://github.com/LuciusChen/blame-reveal
(use-package blame-reveal
  :load-path "~/workspace/blame-reveal"
  ;; :disabled t
  :defer 2
  ;; :bind (:map blame-reveal-mode-map
  ;;             ("f" . blame-reveal-show-file-history)
  ;;             ;; ("n" . blame-reveal-show-line-history)
  ;;             )
  :config
  (setq blame-reveal-recent-days-limit nil)        ; Smart calculation
  (setq blame-reveal-gradient-quality 'strict)         ; Balanced quality
  (setq blame-reveal-display-layout 'none)

  (setq blame-reveal-margin-time-format "%y/%m/%d")
  (setq blame-reveal--margin-width 24)

  (setq blame-reveal-show-uncommitted-fringe t)

  (setq blame-reveal-color-scheme
        '(:hue 210                          ; 0=red, 120=green, 280=purple
               :dark-newest 0.70                 ; Dark: higher = brighter
               :dark-oldest 0.30
               :light-newest 0.35                ; Light: lower = darker
               :light-oldest 0.85
               :saturation-min 0.35 :saturation-max 0.70))

  (require 'blame-reveal-recursive)

  ;; Define custom formatter
  (defun my-minimal-header (commit-hash info color)
    "Minimal: just hash and message"
    (if (string-match-p "^0+$" commit-hash)
        (make-blame-reveal-commit-display
         :lines (list "▸ *Not Committed Yet*")
         :faces (list `(:foreground ,color :weight bold :height 0.8))
         :color color)
      (pcase-let ((`(,hash ,_author ,_date ,msg ,_ts ,_desc) info))
        (make-blame-reveal-commit-display
         :lines (list (format "▸ %s %s (%s): %s" hash _author _date msg))
         :faces (list `(:foreground ,color :weight bold :height 0.8))
         :color color))))

  ;; Apply it
  (setq blame-reveal-header-format-function #'my-minimal-header)
  )

(provide 'init-vcs)
