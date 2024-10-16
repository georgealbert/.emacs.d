;;; init-funcs.el -*- lexical-binding: t; -*-

;; doom-libs.el
(defvar doom--hook nil)
(defun doom-run-hook (hook)
  "Run HOOK (a hook function) with better error handling.
Meant to be used with `run-hook-wrapped'."
  ;; 不记录log了，不然还要加上一堆doom的函数
  ;; (doom-log "hook:%s: run %s" (or doom--hook '*) hook)
  (condition-case-unless-debug e
      (funcall hook)
    (error
     (signal 'doom-hook-error (list hook e))))
  ;; return nil so `run-hook-wrapped' won't short circuit
  nil)

(defun doom-run-hooks (&rest hooks)
  "Run HOOKS (a list of hook variable symbols) with better error handling.
Is used as advice to replace `run-hooks'."
  (dolist (hook hooks)
    (condition-case-unless-debug e
        (let ((doom--hook hook))
          (run-hook-wrapped hook #'doom-run-hook))
      (doom-hook-error
       (unless debug-on-error
         (lwarn hook :error "Error running hook %S because: %s"
                (if (symbolp (cadr e))
                    (symbol-name (cadr e))
                  (cadr e))
                (caddr e)))
       (signal 'doom-hook-error (cons hook (cdr e)))))))

(defun doom-run-hook-on (hook-var trigger-hooks)
  "Configure HOOK-VAR to be invoked exactly once when any of the TRIGGER-HOOKS
are invoked *after* Emacs has initialized (to reduce false positives). Once
HOOK-VAR is triggered, it is reset to nil.

HOOK-VAR is a quoted hook.
TRIGGER-HOOK is a list of quoted hooks and/or sharp-quoted functions."
  (dolist (hook trigger-hooks)
    (let ((fn (make-symbol (format "chain-%s-to-%s-h" hook-var hook)))
          running?)
      (fset
       fn (lambda (&rest _)
            ;; Only trigger this after Emacs has initialized.
            (when (and (not running?)
                       ;; TODO: 考虑后面有时间再加上，应该不会多次触发吧?
                       ;; (not (doom-context-p 'init))
                       (or (daemonp)
                           ;; In some cases, hooks may be lexically unset to
                           ;; inhibit them during expensive batch operations on
                           ;; buffers (such as when processing buffers
                           ;; internally). In that case assume this hook was
                           ;; invoked non-interactively.
                           (and (boundp hook)
                                (symbol-value hook))))
              (setq running? t)  ; prevent infinite recursion
              (doom-run-hooks hook-var)
              (set hook-var nil))))
      (when (daemonp)
        ;; In a daemon session we don't need all these lazy loading shenanigans.
        ;; Just load everything immediately.
        (add-hook 'server-after-make-frame-hook fn 'append))
      (if (eq hook 'find-file-hook)
          ;; Advise `after-find-file' instead of using `find-file-hook' because
          ;; the latter is triggered too late (after the file has opened and
          ;; modes are all set up).
          (advice-add 'after-find-file :before fn '((depth . -101)))
        (add-hook hook fn -101))
      fn)))
;; end of doom-libs.el

(provide 'init-funcs)
