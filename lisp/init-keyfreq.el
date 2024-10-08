;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package keyfreq
  :disabled t
  :defer 5
  :config
  (defun turnon-keyfreq-mode ()
    (interactive)
    (keyfreq-mode 1)
    (keyfreq-autosave-mode 1))

  ;; (defun turnoff-keyfreq-mode ()
  ;;   (interactive)
  ;;   (keyfreq-mode -1)
  ;;   (keyfreq-autosave-mode -1))

  (setq keyfreq-excluded-commands
        '(self-insert-command
          abort-recursive-edit
          ace-jump-done
          ace-jump-move
          ace-window
          avy-goto-line
          backward-char
          backward-kill-word
          backward-word
          clipboard-kill-ring-save
          comint-previous-input
          comint-send-input
          company-complete-common
          company-complete-number
          company-complete-selection
          company-ignore
          delete-backward-char
          describe-variable
          dired ; nothing to optimize in dired
          dired-do-async-shell-command
          dired-find-file
          diredp-next-line
          diredp-previous-line
          electric-pair-delete-pair
          erase-message-buffer
          eval-buffer
          evil-a-WORD
          evil-append
          evil-backward-char
          evil-backward-word-begin
          evil-change
          evil-change-line
          evil-complete-next
          evil-complete-previous
          evil-delete
          evil-delete-backward-char-and-join
          evil-delete-char
          evil-delete-line
          evil-emacs-state
          evil-end-of-line
          evil-escape-emacs-state
          evil-escape-insert-state
          evil-escape-isearch
          evil-escape-minibuffer
          evil-escape-motion-state
          evil-escape-visual-state
          evil-ex
          evil-ex-command
          evil-ex-completion
          evil-ex-delete-backward-char
          evil-exit-emacs-state
          evil-exit-visual-state
          evil-filepath-inner-text-object
          evil-filepath-outer-text-object
          evil-find-char
          evil-find-char-to
          evil-first-non-blank
          evil-force-normal-state
          evil-forward-char
          evil-forward-word-begin
          evil-forward-word-end
          evil-goto-definition
          evil-goto-first-line
          evil-goto-line
          evil-goto-mark-line
          evil-indent
          evil-inner-WORD
          evil-inner-double-quote
          evil-inner-single-quote
          evil-inner-word
          evil-insert
          evil-join
          evil-jump-backward
          evil-jump-forward
          evil-mc-make-and-goto-next-match
          evil-next-line
          evil-next-visual-line
          evil-normal-state
          evil-open-below
          evil-paste-after
          evil-paste-before
          evil-previous-line
          evil-previous-visual-line
          evil-record-macro
          evil-repeat
          evil-replace
          evil-ret
          evil-scroll-page-down
          evil-scroll-page-up
          evil-search-forward
          evil-search-next
          evil-search-word-forward
          evil-set-marker
          evil-substitute
          evil-visual-block
          evil-visual-char
          evil-visual-line
          evil-yank
          exit-minibuffer
          ;; ffip
          forward-char
          forward-word
          goto-line
          ;; hippie-expand
          helm-keyboard-quit
          helm-next-line
          helm-maybe-exit-minibuffer
          ido-complete
          ido-delete-backward-updir
          ido-exit-minibuffer
          ido-switch-buffer
          indent-new-comment-line
          isearch-abort
          isearch-backward-regexp
          isearch-cancel
          isearch-delete-char
          isearch-exit
          isearch-forward-regexp
          isearch-other-control-char
          isearch-other-meta-char
          isearch-printing-char
          isearch-repeat-forward
          isearch-ring-retreat
          ispell-minor-check
          ivy-backward-delete-char
          ivy-backward-kill-word
          ivy-done
          ivy-next-line
          ivy-occur
          ivy-occur-next-line
          ivy-occur-press-and-switch
          ivy-occur-previous-line
          ivy-previous-line
          ivy-wgrep-change-to-wgrep-mode
          keyboard-escape-quit
          keyboard-quit
          keyfreq-mode
          keyfreq-save-now
          keyfreq-show
          kill-sentence
          left-char
          magit-section-forward
          magit-section-backward
          minibuffer-complete
          minibuffer-complete-and-exit
          minibuffer-keyboard-quit
          move-beginning-of-line
          move-end-of-line
          mwheel-scroll
          newline-and-indent
          next-history-element
          next-line
          newline
          org-beginning-of-line
          org-ctrl-c-ctrl-c
          org-cycle
          org-delete-backward-char
          org-end-of-line
          org-force-self-insert
          org-return
          org-self-insert-command
          org-todo
          orgtbl-self-insert-command
          package-menu-execute
          pcomplete
          previous-history-element
          previous-line
          push-button
          pwd
          quit-window
          right-char
          save-buffer
          save-buffers-kill-terminal
          scroll-down-command
          scroll-up-command
          ;; winum-select-window-0
          ;; winum-select-window-1
          ;; winum-select-window-2
          ;; winum-select-window-3
          ;; winum-select-window-4
          ;; winum-select-window-5
          ;; winum-select-window-6
          ;; winum-select-window-7
          ;; winum-select-window-8
          ;; winum-select-window-9
          self-insert-command
          smarter-move-beginning-of-line
          suspend-frame
          term-send-raw
          turnon-keyfreq-mode
          undefined ;; lambda function
          undo-tree-redo
          undo-tree-undo
          wgrep-finish-edit
          yank
          yas-compile-directory
          yas-expand
          yas-next-field-or-maybe-expand
          ))

  (turnon-keyfreq-mode)
  (unless (file-exists-p (file-truename keyfreq-file))
    (with-temp-buffer
      (insert "()")
      (write-file (file-truename keyfreq-file))))
  )

;; And use keyfreq-show to see how many times you used a command.
;; It's recommended to use `keyfreq-mode' (could be in "~/.custom.el").
;; It's reported keyfreq is not compatible with `latex-mode'
;; @see https://github.com/redguardtoo/emacs.d/issues/767
;; (turnon-keyfreq-mode)

(provide 'init-keyfreq)
