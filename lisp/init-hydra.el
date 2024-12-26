;;; init-hydra.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; find-file-in-project
(eval-after-load 'find-file-in-project
  '(progn
     (defhydra hydra-ffip-diff-group (:color blue)
       "
[_k_] Previous hunk
[_j_] Next hunk
[_p_] Previous file
[_n_] Next file
"
       ("k" diff-hunk-prev)
       ("j" diff-hunk-next)
       ("p" diff-file-prev)
       ("n" diff-file-next)
       ("q" nil))))

(defun ffip-diff-mode-hook-hydra-setup ()
  (local-set-key (kbd "C-c C-y") 'hydra-ffip-diff-group/body))

(add-hook 'ffip-diff-mode-hook 'ffip-diff-mode-hook-hydra-setup)


;; {{ dired
;; (eval-after-load 'dired
;;   '(progn
;;      (defun my-replace-dired-base (base)
;;        "Change file name in `wdired-mode'"
;;        (let* ((fp (dired-file-name-at-point))
;;               (fb (file-name-nondirectory fp))
;;               (ext (file-name-extension fp))
;;               (dir (file-name-directory fp))
;;               (nf (concat base "." ext)))
;;          (when (yes-or-no-p (format "%s => %s at %s?"
;;                                     fb nf dir))
;;            (rename-file fp (concat dir nf)))))

;;      (defun my-copy-file-info (fn)
;;        (message "%s => clipboard & yank ring"
;;                 (kill-new (funcall fn (dired-file-name-at-point)))))

;;      (defhydra hydra-dired (:color blue)
;;        "
;; ^File^             ^Copy Info^
;; ------------------------------------
;; [_R_] Move         [_pp_] Path
;; [_cf_] New         [_nn_] Name
;; [_rr_] Rename      [_bb_] Base
;; [_ff_] Find        [_dd_] directory
;; [_C_]  Copy
;; [_rb_] Change base
;; [_+_] Create directory
;; "
;;        ("pp" (my-copy-file-info 'file-truename))
;;        ("nn" (my-copy-file-info 'file-name-nondirectory))
;;        ("bb" (my-copy-file-info 'file-name-base))
;;        ("dd" (my-copy-file-info 'file-name-directory))
;;        ("rb" (my-replace-dired-base (car kill-ring)))
;;        ("cc" my-dired-redo-last-command)
;;        ("C" dired-do-copy)
;;        ("R" dired-rename-file)
;;        ("cf" find-file)
;;        ("rr" dired-toggle-read-only)
;;        ("ff" (lambda (regexp)
;;                (interactive "sMatching regexp: ")
;;                (find-lisp-find-dired default-directory regexp)))
;;        ("+" dired-create-directory)
;;        ("q" nil))))

;; (defun dired-mode-hook-hydra-setup ()
;;   (local-set-key (kbd "y") 'hydra-dired/body))

;; (add-hook 'dired-mode-hook 'dired-mode-hook-hydra-setup)
;; }}

(use-package pretty-hydra
  :custom (pretty-hydra-default-title-body-format-spec " %s%s")
  :bind ("<f8>" . toggles-hydra/body)
  :hook (emacs-lisp-mode . (lambda ()
                             (add-to-list
                              'imenu-generic-expression
                              '("Hydras"
                                "^.*(\\(pretty-hydra-define\\) \\([a-zA-Z-]+\\)"
                                2))))
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:inherit highlight :inverse-video t)))
          (height (or height 1.2))
          (v-adjust (or v-adjust 0.0)))
      (concat
       ;; (when (and (icons-displayable-p) icon-type icon-name)
       (when (and icon-type icon-name)
         (let ((f (intern (format "nerd-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face))))

  ;; Global toggles
  (with-no-warnings
    (pretty-hydra-define toggles-hydra (:title (pretty-hydra-title "Toggles" 'faicon "nf-fa-toggle_on")
                                        :color amaranth :quit-key ("q" "C-g"))
      ("Basic"
       (("n" (cond ((fboundp 'display-line-numbers-mode)
                    (display-line-numbers-mode (if display-line-numbers-mode -1 1)))
                   ((fboundp 'gblobal-linum-mode)
                    (global-linum-mode (if global-linum-mode -1 1))))
         "line number"
         :toggle (or (bound-and-true-p display-line-numbers-mode)
                     (bound-and-true-p global-linum-mode)))
        ("a" global-aggressive-indent-mode "aggressive indent" :toggle t)
        ("d" global-hungry-delete-mode "hungry delete" :toggle t)
        ("e" electric-pair-mode "electric pair" :toggle t)
        ("c" flyspell-mode "spell check" :toggle t)
        ("s" prettify-symbols-mode "pretty symbol" :toggle t)
        ("l" global-page-break-lines-mode "page break lines" :toggle t)
        ("b" display-battery-mode "battery" :toggle t)
        ("i" display-time-mode "time" :toggle t)
        ("m" doom-modeline-mode "modern mode-line" :toggle t))
       "Highlight"
       (("h l" global-hl-line-mode "line" :toggle t)
        ("h p" show-paren-mode "paren" :toggle t)
        ("h s" symbol-overlay-mode "symbol" :toggle t)
        ("h r" rainbow-mode "rainbow" :toggle t)
        ("h w" (setq-default show-trailing-whitespace (not show-trailing-whitespace))
         "whitespace" :toggle show-trailing-whitespace)
        ("h d" rainbow-delimiters-mode "delimiter" :toggle t)
        ("h i" highlight-indent-guides-mode "indent" :toggle t)
        ("h t" global-hl-todo-mode "todo" :toggle t))
       "Program"
       (("f" flymake-mode "flymake" :toggle t)
        ("O" hs-minor-mode "hideshow" :toggle t)
        ("u" subword-mode "subword" :toggle t)
        ("W" which-function-mode "which function" :toggle t)
        ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
        ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit))
        ("v" global-diff-hl-mode "gutter" :toggle t)
        ("V" diff-hl-flydiff-mode "live gutter" :toggle t)
        ("M" diff-hl-margin-mode "margin gutter" :toggle t)
        ("D" diff-hl-dired-mode "dired gutter" :toggle t))
       ))))

(provide 'init-hydra)
