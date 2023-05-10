;;; init-erc.el -*- lexical-binding: t; -*-

;; Require ERC-SASL package

;; https://github.com/syl20bnr/spacemacs/blob/master/layers/%2Bchat/erc/local/erc-sasl/erc-sasl.el
(use-package erc-sasl
  :after erc
  :load-path "~/.emacs.d/site-lisp/extensions/erc-sasl"
  :bind (("C-c e" . erc-track-switch-buffer))
  :config
  (setq erc-fill-column 150)

  ;; utf-8 always and forever
  (setq erc-server-coding-system '(utf-8 . utf-8))

  ;; Interpret mIRC-style color commands in IRC chats
  (setq erc-interpret-mirc-color t)

  ;; Kill buffers for channels after /part
  (setq erc-kill-buffer-on-part t)

  ;; Kill buffers for private queries after quitting the server
  (setq erc-kill-queries-on-quit t)

  ;; Kill buffers for server messages after quitting the server
  (setq erc-kill-server-buffer-on-quit t)

  ;; exclude boring stuff from tracking
  (erc-track-mode t)

  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477"))

  (setq erc-autojoin-channels-alist '(("Libera.Chat" "#emacs" "#linuxba")))

  (setq erc-channel-hide-list '(("#linuxba" "JOIN" "PART" "QUIT" "NICK" "324" "329" "332" "333" "353" "477")
                                ("#neovim" "JOIN" "PART" "QUIT")
                                ("#emacs" "JOIN" "PART" "QUIT" "NICK")))

  ;; Add SASL server to list of SASL servers (start a new list, if it did not exist)
  (add-to-list 'erc-sasl-server-regexp-list "irc\\.libera\\.chat")

  ;; Redefine/Override the erc-login() function from the erc package, so that
  ;; it now uses SASL
  (defun erc-login ()
    "Perform user authentication at the IRC server. (PATCHED)"
    (erc-log (format "login: nick: %s, user: %s %s %s :%s"
                     (erc-current-nick)
                     (user-login-name)
                     (or erc-system-name (system-name))
                     erc-session-server
                     erc-session-user-full-name))
    (if erc-session-password
        (erc-server-send (format "PASS %s" erc-session-password))
      (message "Logging in without password"))
    (when (and (featurep 'erc-sasl) (erc-sasl-use-sasl-p))
      (erc-server-send "CAP REQ :sasl"))
    (erc-server-send (format "NICK %s" (erc-current-nick)))
    (erc-server-send
     (format "USER %s %s %s :%s"
             ;; hacked - S.B.
             (if erc-anonymous-login erc-email-userid (user-login-name))
             "0" "*"
             erc-session-user-full-name))
    (erc-update-mode-line)))

(defun start-libera ()
  "Connect to irc.libera.chat."
  (interactive)

  (require 'erc)
  (require 'erc-sasl)
  
  (defun read-lines (filePath)
    "Return a list of lines of a file at filePath."
    (with-temp-buffer (insert-file-contents filePath)
                      (split-string (buffer-string) "\n" t)))

  (when (y-or-n-p "Do you want to start IRC on irc.libera.chat? ")
    (let* ((acc (read-lines "~/.irc-account"))
           (irc-nick (car acc))
           (irc-password (nth 1 acc)))
      (erc-tls :server "irc.libera.chat" :port 6697 :nick irc-nick
               :full-name irc-nick
               :password irc-password))))

(provide 'init-erc)
