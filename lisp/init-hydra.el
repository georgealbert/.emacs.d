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
(eval-after-load 'dired
  '(progn
     (defun my-replace-dired-base (base)
       "Change file name in `wdired-mode'"
       (let* ((fp (dired-file-name-at-point))
              (fb (file-name-nondirectory fp))
              (ext (file-name-extension fp))
              (dir (file-name-directory fp))
              (nf (concat base "." ext)))
         (when (yes-or-no-p (format "%s => %s at %s?"
                                    fb nf dir))
           (rename-file fp (concat dir nf)))))

     (defun my-copy-file-info (fn)
       (message "%s => clipboard & yank ring"
                (kill-new (funcall fn (dired-file-name-at-point)))))

     (defhydra hydra-dired (:color blue)
       "
^File^             ^Copy Info^
------------------------------------
[_R_] Move         [_pp_] Path
[_cf_] New         [_nn_] Name
[_rr_] Rename      [_bb_] Base
[_ff_] Find        [_dd_] directory
[_C_]  Copy
[_rb_] Change base
[_+_] Create directory
"
       ("pp" (my-copy-file-info 'file-truename))
       ("nn" (my-copy-file-info 'file-name-nondirectory))
       ("bb" (my-copy-file-info 'file-name-base))
       ("dd" (my-copy-file-info 'file-name-directory))
       ("rb" (my-replace-dired-base (car kill-ring)))
       ("cc" my-dired-redo-last-command)
       ("C" dired-do-copy)
       ("R" dired-rename-file)
       ("cf" find-file)
       ("rr" dired-toggle-read-only)
       ("ff" (lambda (regexp)
               (interactive "sMatching regexp: ")
               (find-lisp-find-dired default-directory regexp)))
       ("+" dired-create-directory)
       ("q" nil))))

(defun dired-mode-hook-hydra-setup ()
  (local-set-key (kbd "y") 'hydra-dired/body))

(add-hook 'dired-mode-hook 'dired-mode-hook-hydra-setup)
;; }}

(provide 'init-hydra)
