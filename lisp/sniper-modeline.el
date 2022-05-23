;;; sniper-modeline.el --- A minimal mode-line inspired by doom-modeline -*- lexical-binding: t; -*-

;; Author: sniper
;; Homepage: https://gitee.com/e190/emacs.d
;; Keywords: dashboard
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; sniper-modeline is a minimal mode-line configuration that aims to replicate
;; some of the features of the doom-modeline package.
;;
;; Features offered:
;; * Clean, minimal design
;; * Anzu and multiple-cursors counter
;; * Version control status indicator
;; * Flycheck status indicator
;; * Flymake support
;; * Lightweight with no dependencies
;;
;; To enable sniper-modeline:
;; (sniper-modeline-mode)

;;; License:
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Code:

;;
;; Variable declarations
;;

(defvar flycheck-current-errors)
(defvar flymake--mode-line-format)
(defvar anzu-cons-mode-line-p)
(defvar anzu--state)
(defvar anzu--cached-count)
(defvar anzu--overflow-p)
(defvar anzu--current-position)
(defvar anzu--total-matched)
(defvar multiple-cursors-mode)

;;
;; Function prototypes
;;

(declare-function flycheck-count-errors "flycheck" (errors))
(declare-function mc/num-cursors "multiple-cursors" ())

;;
;; Config
;;

(defgroup sniper-modeline nil
  "A minimal mode-line configuration inspired by doom-modeline."
  :group 'mode-line)

(defcustom sniper-modeline-show-cursor-point nil
  "If t, the value of `point' will be displayed next to the cursor position in the mode-line."
  :group 'sniper-modeline
  :type 'boolean)

(defcustom sniper-modeline-buffer-name-full nil
  "If t, the value of `point' will be displayed next to the cursor position in the mode-line."
  :group 'sniper-modeline
  :type 'boolean)

(defface sniper-modeline-buffer-name
  '((t (:inherit (mode-line-buffer-id))))
  "Face used for major mode indicator in the mode-line."
  :group 'sniper-modeline)

(defface sniper-modeline-major-mode
  '((t (:inherit (bold))))
  "Face used for major mode indicator in the mode-line."
  :group 'sniper-modeline)

(defface sniper-modeline-status-neutral
  '((t (:inherit (shadow))))
  "Face used for neutral or inactive status indicators in the mode-line."
  :group 'sniper-modeline)

(defface sniper-modeline-status-info
  '((t (:inherit (font-lock-keyword-face))))
  "Face used for generic status indicators in the mode-line."
  :group 'sniper-modeline)

(defface sniper-modeline-status-success
  '((t (:inherit (success))))
  "Face used for success status indicators in the mode-line."
  :group 'sniper-modeline)

(defface sniper-modeline-status-warning
  '((t (:inherit (warning))))
  "Face for warning status indicators in the mode-line."
  :group 'sniper-modeline)

(defface sniper-modeline-status-error
  '((t (:inherit (error))))
  "Face for error stauts indicators in the mode-line."
  :group 'sniper-modeline)

(defface sniper-modeline-unimportant
  '((t (:inherit (shadow))))
  "Face used for less important mode-line elements."
  :group 'sniper-modeline)

(defface sniper-modeline-modified
  '((t (:inherit (error))))
  "Face used for the 'modified' indicator symbol in the mode-line."
  :group 'sniper-modeline)

(defface sniper-modeline-lsp-success
  '((t (:inherit success :weight normal)))
  "Face for LSP success state.")

(defface sniper-modeline-lsp-warning
  '((t (:inherit warning :weight normal)))
  "Face for LSP warning state.")

(defface sniper-modeline-lsp-error
  '((t (:inherit error :weight normal)))
  "Face for LSP error state.")

(defface sniper-modeline-lsp-running
  '((t (:inherit compilation-mode-line-run :weight normal :slant normal)))
  "Face for LSP running state.")
(declare-function lsp-workspaces "ext:lsp-mode")

(defface sniper-modeline-evil-emacs-state
  '((t (:inherit (font-lock-builtin-face bold))))
  "Face for the Emacs state tag in evil state indicator.")

(defface sniper-modeline-evil-insert-state
  '((t (:inherit (font-lock-keyword-face bold))))
  "Face for the insert state tag in evil state indicator.")

(defface sniper-modeline-evil-motion-state
  '((t :inherit (font-lock-doc-face bold) :slant normal))
  "Face for the motion state tag in evil state indicator.")

(defface sniper-modeline-evil-normal-state
  '((t (:inherit (success bold))))
  "Face for the normal state tag in evil state indicator.")

(defface sniper-modeline-evil-operator-state
  '((t (:inherit (mode-line-buffer-id bold))))
  "Face for the operator state tag in evil state indicator.")

(defface sniper-modeline-evil-visual-state
  '((t (:inherit (warning bold))))
  "Face for the visual state tag in evil state indicator.")

(defface sniper-modeline-evil-replace-state
  '((t (:inherit (error bold))))
  "Face for the replace state tag in evil state indicator.")

(defvar evil-state)
(declare-function evil-emacs-state-p "ext:evil-states" t t)
(declare-function evil-force-normal-state "ext:evil-commands" t t)
(declare-function evil-insert-state-p "ext:evil-states" t t)
(declare-function evil-motion-state-p "ext:evil-states" t t)
(declare-function evil-normal-state-p "ext:evil-states" t t)
(declare-function evil-operator-state-p "ext:evil-states" t t)
(declare-function evil-replace-state-p "ext:evil-states" t t)
(declare-function evil-state-property "ext:evil-common")
(declare-function evil-visual-state-p "ext:evil-states" t t)

;;
;; Helper functions
;;

(defun sniper-modeline--string-trim-left (string)
  "Remove whitespace at the beginning of STRING."
  (if (string-match "\\`[ \t\n\r]+" string)
      (replace-match "" t t string)
    string))

(defun sniper-modeline--string-trim-right (string)
  "Remove whitespace at the end of STRING."
  (if (string-match "[ \t\n\r]+\\'" string)
      (replace-match "" t t string)
    string))

(defun sniper-modeline--string-trim (string)
  "Remove whitespace at the beginning and end of STRING."
  (sniper-modeline--string-trim-left (sniper-modeline--string-trim-right string)))

(defun sniper-modeline--format (left right)
  "Return a string of `window-width' length containing LEFT and RIGHT, aligned respectively."
  (let ((reserve (length right)))
    (concat left
            " "
            (propertize " "
                        'display `((space :align-to (- right (- 0 right-margin) ,reserve))))
            right)))

;;
;; Update functions
;;

(defvar-local sniper-modeline--vc-text nil)
(defun sniper-modeline--update-vc-segment (&rest _)
  "Update `sniper-modeline--vc-text' against the current VCS state."
  (setq sniper-modeline--vc-text
        (when (and vc-mode buffer-file-name)
          (let ((backend (vc-backend buffer-file-name))
                (state (vc-state buffer-file-name (vc-backend buffer-file-name))))
            (let ((face 'mode-line-neutral))
              (concat (cond ((memq state '(edited added))
                             (setq face 'sniper-modeline-status-info)
                             (propertize "+" 'face face))
                            ((eq state 'needs-merge)
                             (setq face 'sniper-modeline-status-warning)
                             (propertize "⟷" 'face face))
                            ((eq state 'needs-update)
                             (setq face 'sniper-modeline-status-warning)
                             (propertize "↑" 'face face))
                            ((memq state '(removed conflict unregistered))
                             (setq face 'sniper-modeline-status-error)
                             (propertize "✖" 'face face))
                            (t
                             (setq face 'sniper-modeline-status-neutral)
                             (propertize "✔" 'face face)))
                      (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                                  'face face
                                  'mouse-face face)
                      " "))))))

(defvar-local sniper-modeline--flycheck-text nil)
(defun sniper-modeline--update-flycheck-segment (&optional status)
  "Update `sniper-modeline--flycheck-text' against the reported flycheck STATUS."
  (setq sniper-modeline--flycheck-text
        (pcase status
          ('finished (if flycheck-current-errors
                         (let-alist (flycheck-count-errors flycheck-current-errors)
                           (let ((sum (+ (or .error 0) (or .warning 0))))
                             (propertize (concat "! "
                                                 (number-to-string sum)
                                                 "  ")
                                         'face (if .error
                                                   'sniper-modeline-status-error
                                                 'sniper-modeline-status-warning))))
                       (propertize "✔ " 'face 'sniper-modeline-status-success)))
          ('running (propertize "Δ Checking  " 'face 'sniper-modeline-status-info))
          ('errored (propertize "✖ Error  " 'face 'sniper-modeline-status-error))
          ('interrupted (propertize "⏸ Paused  " 'face 'sniper-modeline-status-neutral))
          ('no-checker ""))))

;;
;; Segments
;;

(defun sniper-modeline-segment-modified ()
  "Displays a color-coded buffer modification/read-only indicator in the mode-line."
  (if (not (string-match-p "\\*.*\\*" (buffer-name)))
      (if (buffer-modified-p)
          (propertize "%1*" 'face 'sniper-modeline-modified)
        (if (and buffer-read-only (buffer-file-name))
            (propertize "■" 'face 'sniper-modeline-unimportant)
          " "))
    "  "))

(defun sniper-modeline-segment-buffer-name ()
  "Displays the name of the current buffer in the mode-line."
  (let ((text (if sniper-modeline-buffer-name-full buffer-file-name "%b"))
        (face (if (buffer-modified-p) 'sniper-modeline-evil-replace-state 'bold)))
    (propertize (sniper-modeline--modal-text text face nil)
                'mouse-face 'mode-line-highlight 'help-echo buffer-file-name)))

(defun sniper-modeline-segment-anzu ()
  "Displays color-coded anzu status information in the mode-line (if available)."
  (when (and (boundp 'anzu--state) anzu--state)
    (cond ((eq anzu--state 'replace-query)
           (format #("Replace: %d  " 0 11 (face sniper-modeline-status-warning)) anzu--cached-count))
          (anzu--overflow-p
           (format #("%d/%d+  " 0 3 (face sniper-modeline-status-info) 3 6 (face sniper-modeline-status-error)) anzu--current-position anzu--total-matched))
          (t
           (format #("%d/%d  " 0 5 (face sniper-modeline-status-info)) anzu--current-position anzu--total-matched)))))

(defun sniper-modeline-segment-multiple-cursors ()
  "Displays the number of active multiple-cursors in the mode-line (if available)."
  (when (and (boundp 'multiple-cursors-mode) multiple-cursors-mode)
    (concat "MC"
            (format #("×%d  " 0 3 (face sniper-modeline-status-warning)) (mc/num-cursors)))))

(defun sniper-modeline-segment-position ()
  "Displays the current cursor position in the mode-line."
  (concat "%l:%c"
          (when sniper-modeline-show-cursor-point (propertize (format ":%d" (point)) 'face 'sniper-modeline-unimportant))
          (propertize " %p%%  " 'face 'sniper-modeline-unimportant)))

(defun sniper-modeline-segment-encoding ()
  "Displays the encoding and EOL style of the buffer in the mode-line."
 (propertize
  (concat (pcase (coding-system-eol-type buffer-file-coding-system)
            (0 "LF ")
            (1 "CRLF ")
            (2 "CR "))
          (let ((sys (coding-system-plist buffer-file-coding-system)))
            (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                   "UTF-8")
                  (t (upcase (symbol-name (plist-get sys :name))))))
          )
  ;; 'face 'bold
  'mouse-face 'mode-line-highlight
  'help-echo (format "End-of-line style: %s\nmouse-1: Cycle"
                      (pcase (coding-system-eol-type buffer-file-coding-system)
                        (0 "Unix-style LF")
                        (1 "DOS-style CRLF")
                        (2 "Mac-style CR")
                        (_ "Undecided")))
  'local-map (let ((map (make-sparse-keymap)))
                (define-key map [mode-line mouse-1] 'mode-line-change-eol)
                map)))

(defun sniper-modeline-segment-vc ()
  "Displays color-coded version control information in the mode-line."
  sniper-modeline--vc-text)

(defun sniper-modeline-segment-major-mode ()
 "Displays the current major mode in the mode-line."
 (propertize
  (concat (or (and (boundp 'delighted-modes)
                   (cadr (assq major-mode delighted-modes)))
              (format-mode-line mode-name)))
  'face 'sniper-modeline-evil-normal-state))

(defun sniper-modeline-segment-misc-info ()
  "Displays the current value of `mode-line-misc-info' in the mode-line."
  (let ((misc-info (format-mode-line mode-line-misc-info 'sniper-modeline-unimportant)))
    (unless (string= (sniper-modeline--string-trim misc-info) "")
      (concat (sniper-modeline--string-trim misc-info) "  "))))

(defun sniper-modeline-segment-flycheck ()
  "Displays color-coded flycheck information in the mode-line (if available)."
  sniper-modeline--flycheck-text)

(defun sniper-modeline-segment-flymake ()
  "Displays information about the current status of flymake in the mode-line (if available)."
  (when (and (boundp 'flymake-mode) flymake-mode)
    ;; Depending on Emacs version, flymake stores the mode-line segment using one of two variable names
    (let ((flymake-segment-format (if (boundp 'flymake-mode-line-format)
                                      flymake-mode-line-format
                                    flymake--mode-line-format)))
      (concat (sniper-modeline--string-trim (format-mode-line flymake-segment-format)) "  "))))

(defun sniper-modeline-segment-process ()
  "Displays the current value of `mode-line-process' in the mode-line."
  (let ((process-info (format-mode-line mode-line-process)))
    (unless (string= (sniper-modeline--string-trim process-info) "")
      (concat (sniper-modeline--string-trim process-info) "  "))))

(defun sniper-modeline--modal-text (text face help-echo)
  "Display the model icon with FACE and HELP-ECHO.
TEXT is alternative if icon is not available."
  (propertize text 'face face 'help-echo help-echo))

(defun sniper-modeline-segment-evil-modal ()
  "The current evil state. Requires `evil-mode' to be enabled."
  (when (bound-and-true-p evil-local-mode)
    (sniper-modeline--modal-text
     (let ((tag (evil-state-property evil-state :tag t)))
       (if (stringp tag) tag (funcall tag)))
     (cond
      ((evil-normal-state-p) 'sniper-modeline-evil-normal-state)
      ((evil-emacs-state-p) 'sniper-modeline-evil-emacs-state)
      ((evil-insert-state-p) 'sniper-modeline-evil-insert-state)
      ((evil-motion-state-p) 'sniper-modeline-evil-motion-state)
      ((evil-visual-state-p) 'sniper-modeline-evil-visual-state)
      ((evil-operator-state-p) 'sniper-modeline-evil-operator-state)
      ((evil-replace-state-p) 'sniper-modeline-evil-replace-state)
      (t 'sniper-modeline-evil-normal-state))
     (evil-state-property evil-state :name t))))

(defun sniper-modeline-segment-buffer-size ()
  "Show buffer size."
  (propertize "%I" 'face 'mode-line
                    'mouse-face 'mode-line-highlight))

(defvar-local sniper-modeline--lsp nil)
(defun sniper-modeline-update-lsp (&rest _)
  "Update `lsp-mode' state."
  (setq sniper-modeline--lsp
        (let* ((workspaces (if (bound-and-true-p lsp-mode) (lsp-workspaces)))
               (face (if workspaces 'sniper-modeline-lsp-success 'sniper-modeline-lsp-warning))
               (icon (sniper-modeline--modal-text "LSP" face nil)))
          (if (bound-and-true-p lsp-mode) (propertize icon
                      'mouse-face 'mode-line-highlight))
          )))

(add-hook 'lsp-before-initialize-hook #'sniper-modeline-update-lsp)
(add-hook 'lsp-after-initialize-hook #'sniper-modeline-update-lsp)
(add-hook 'lsp-after-uninitialized-functions #'sniper-modeline-update-lsp)
(add-hook 'lsp-before-open-hook #'sniper-modeline-update-lsp)
(add-hook 'lsp-after-open-hook #'sniper-modeline-update-lsp)

;;
;; Activation function
;;

(defvar-local sniper-modeline--default-mode-line mode-line-format)
(defvar-local sniper-modeline--anzu-cons-mode-line-p nil)

;;;###autoload
(define-minor-mode sniper-modeline-mode
  "Toggle sniper-modeline on or off."
  :group 'sniper-modeline
  :global t
  :lighter nil
  (if sniper-modeline-mode
      (progn

        ;; Setup flycheck hooks
        (add-hook 'flycheck-status-changed-functions #'sniper-modeline--update-flycheck-segment)
        (add-hook 'flycheck-mode-hook #'sniper-modeline--update-flycheck-segment)

        ;; Setup VC hooks
        (add-hook 'find-file-hook #'sniper-modeline--update-vc-segment)
        (add-hook 'after-save-hook #'sniper-modeline--update-vc-segment)
        (advice-add #'vc-refresh-state :after #'sniper-modeline--update-vc-segment)

        ;; Disable anzu's mode-line segment setting, saving the previous setting to be restored later (if present)
        (when (boundp 'anzu-cons-mode-line-p)
          (setq sniper-modeline--anzu-cons-mode-line-p anzu-cons-mode-line-p))
        (setq-default anzu-cons-mode-line-p nil)

        ;; Save previous mode-line-format to be restored later
        (setq sniper-modeline--default-mode-line mode-line-format)

        ;; Set the new mode-line-format
        (setq-default mode-line-format
                      '((:eval
                         (sniper-modeline--format
                          ;; Left
                          (format-mode-line
                           '(" "
                             (:eval (sniper-modeline-segment-evil-modal))
                             ;; "  "
                             ;; (:eval (sniper-modeline-segment-buffer-size))
                             "  "
                             (:eval (sniper-modeline-segment-modified))
                             (:eval (sniper-modeline-segment-buffer-name))
                             "  "
                             ;; (:eval (sniper-modeline-segment-anzu))
                             ;; (:eval (sniper-modeline-segment-multiple-cursors))
                             (:eval (sniper-modeline-segment-position))))

                          ;; Right
                          (format-mode-line
                           '((:eval (sniper-modeline-update-lsp))
                             "  "
                             (:eval (sniper-modeline-segment-encoding))
                             " | "
                             (:eval (sniper-modeline-segment-major-mode))
                             "  "
                             (:eval (sniper-modeline-segment-vc))
                             (:eval (sniper-modeline-segment-misc-info))
                             (:eval (sniper-modeline-segment-flycheck))
                             (:eval (sniper-modeline-segment-flymake))
                             (:eval (sniper-modeline-segment-process))
                             " ")))))))
    (progn

      ;; Remove flycheck hooks
      (remove-hook 'flycheck-status-changed-functions #'sniper-modeline--update-flycheck-segment)
      (remove-hook 'flycheck-mode-hook #'sniper-modeline--update-flycheck-segment)

      ;; Remove VC hooks
      (remove-hook 'file-find-hook #'sniper-modeline--update-vc-segment)
      (remove-hook 'after-save-hook #'sniper-modeline--update-vc-segment)
      (advice-remove #'vc-refresh-state #'sniper-modeline--update-vc-segment)

      ;; Restore anzu's mode-line segment setting
      (setq-default anzu-cons-mode-line-p sniper-modeline--anzu-cons-mode-line-p)

      ;; Restore the original mode-line format
      (setq-default mode-line-format sniper-modeline--default-mode-line))))

;;
;; Provide sniper-modeline
;;

(provide 'sniper-modeline)

;;; sniper-modeline.el ends here
