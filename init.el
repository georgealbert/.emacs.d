;; This sets up the load path so that we can override it

;; [2018-11-29 周四 22:28:22] 测试emacs启动需要30s+的问题
;; (toggle-debug-on-error)

;; [2018-11-30 周五 11:45:50] 这个函数是必须的，否则启动报错。
(package-initialize nil)

;; Override the packages with the git version of Org and other packages
;(add-to-list 'load-path "~/.emacs.d/lisp/org-8.2.7c/lisp")
;(add-to-list 'load-path "~/.emacs.d/lisp/org-8.2.7c/contrib/lisp")

;; Load the rest of the packages
(package-initialize t)
(setq package-enable-at-startup nil)
(require 'benchmark-init-modes)
(require 'benchmark-init)
(benchmark-init/activate)
(require 'org)
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "~/.emacs.d/Albert.org"))
(org-babel-load-file (expand-file-name "~/.emacs.d/Albert_org_config.org"))

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(column-number-mode t)
;;  '(custom-safe-themes
;;    (quote
;;     ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
;;  '(display-time-mode t)
;;  '(package-selected-packages
;;    (quote
;;     (benchmark-init elpy ox-publish xah-find color-theme-solarized paren-face evil-paredit paredit window-numbering web-mode use-package powerline org2blog magit htmlize highlight-parentheses helm-swoop evil dired+)))
;;  '(show-paren-mode t)
;;  '(tool-bar-mode nil)
;; ;; '(vc-handled-backends (quote (Git)))
;;  )
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button))))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#181a26" "#FF0000" "#4eee94" "#FFFF00" "#0000cd" "#f08080" "#00FFFF" "#cccccc"])
 '(custom-safe-themes
   (quote
    ("06940a9a3d4e2a9b9f6658465bb193c805b9d05cdc0dd9426038ad317a10e973" "2125149cf24be3d641e40fa490d02525983892ed22f35eb8cf7dbb8eaf766f4d" "d021b87d14fa1dddc79f361c71e27c299f8414dd24d9dda79a413320f74d82c5" "d44d407765074e78658abdf87d5e9bd1cc5a2e18daca9cbf78332b390c7e98f4" "facd7fda971ef0cbca0f6b1deccdf337ae22d49d01fcbde1e6219ff35dc87679" "aa0c4eedcbd40077b18339c5143848ab38bfe6f9bb67373ec9d63f45c07a164f" "7dc66d4535e8519d3310777cba579a6e8a56a18bfcaec62f68db5026ebeddb9f" "dfd630d4f860d87483394e9cc69f300bd6195e2251b58fc87dc2ac96abf44359" "d44de0bd0665897e67d61a874e697c0035201d93a526c226427adf4ac048f021" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "d401a77fab622afb9c5a5cddf75c9079496c03a414c804ccb7d4c1ae53d2b805" "8101eaa00fff062679b472017ddc09a4942e2f5626cde3182325e02cf0f63fdd" "b260a25d4ddfd4f9898efdff1715f470470f2052eb292ccb54ea104ee5510ceb" "b77286aa5889fb04efef9239bc5fdb53998b8d7a55d5e22d49b03a241f9a3be2" "a762fc23d9e0f43f0ecd925ae5b2b90bf3e5ef91b862f3defdc296dd16f68e38" "1c08407bfd92c8eda838d7b540cd72fba1a9751b569c5c84ff49c46af5b2a80f" "7dc5307bb74eb8f0d9803b24a5066d453b909b27242fbf71acc701c17bd5e0ca" "669823c2911bdc30edb4ec0b822aab3f3d15a666f7fdd16c5fb2c617330d87f8" "311ded2658874c5db5e0ca824a7935926c9c4832fb0396a28f6a2de7c4fc45cc" "d6877e375a04421dbafc87e2dd9f3df44d3d2490b06a894304a662f3daba6fde" "85323fcd9fce1305459b082ad8b00db0aec623a6577dbb504e316070e28ba051" "9b2bd936e38de2ae1eb7c49de3097e401e4744c4c15ddc7e22a77acd51f77b1c" "79e72b94128fb0f464f5f9e13ca1b03eda8ef9a29df048d4b3b91f0023057c56" "97c237b0846ee1b28ddd24f11890960bf6ff953e8d41b08950c3917c578bf174" "fd1fa24eeaf8b07ad41069f1098affacb5b7b48f05052adaec858bae294e2a0b" "d29f064c2f15a7a6b9fd95dfa78cb44a61a8657edd3fa3d107c7b15c40bb9501" "1fb9a8b0656050b61cf0182442f4ab89c04bcd4603b3362c286f9c073643c65e" "161ae7ce90d2cc6d506c0ecb103c493c9dc1307039609c6fe53c6032e4456a49" "21b1e1f9b0c6022ffc7ec5129dfe7831647874c6c6ceeb14fbbd3e221c40d561" "48eaaf1fc8750dabaafaef4d56e7371b892b6dfb40867d377734800de41e5e48" "b7ab29df8f04b18fd9daccfa83f5546bbd8a56896b950386eba9078fa149c70b" "43eb917bd7fcc01b28cca2012d18052b8d9172265b2723836a73b3350ce2db7c" "20d2d84db70161c483ee17ebad6ae3760319bb8a6bc441c2e017c85909587ea4" "35db46e987b856a6fc26f6c7113b669a0d07383fa51fcab0bd6ff5559d637275" "38097a45d81709c97b8e80d7d21c563dd73ce7abdf4a3a317598a99390d13330" "a7a2f53162f9b4264c7b35787d4ea02d5fa377eefcb8fce904434abbeb531465" "0c07fc6b14957180f5bddac054b13fd1f53e3d97c1422ea16301e256f8ddb782" "316146d59f659a9b08c25c1ecdf3203cabc8290d1ba790cc1b022b010be10325" "5fe543c3294a1b246227d3709e127b9127c3398581b118249f67118081561937" "5f1f68fe145b5385296a8aa3d2441b854618f2be4bc94e86014ee6e0e4e12c97" "5f87e589063e7f305b403c54da149a9978c94de1cc236f70fa4999094890563b" "76649a18830aa7373765d4cd63036287b2f4abe27779687bc3d79eb15220a418" "8535a5c59cda7ca332f649e9678ea98dbaa76401460a886ed3d07d75ded5bac0" "0aa9ee9f7e279f0b2b25859a88f6ead899a2085ae4689b46ad24e47a4899a9fb" "b5358d4882be006dcd875d275d55625f0c7b0c58f6e1ec85baf2c7dca6f04ff3" "5ba08a9bc975b9f30d1e1b2afd9caa039233c0a4608838f0e29fe7861f15edfe" "5cf3aaaf41b2c60812decd92a4eb48bda21e5da341a2e85a39447f17b45ce20a" "487ae8f3b4c226aea427c77d612efe1c9bde2632425858c56de55d0b9bbfbbdd" "d82481087f6858b687c4bba1ae22c44eac41133dd790511e51afd52ee3f554f9" "4f10d9545d370936aaa816f94045373809ddb3992151400fdb550ee9879a1497" "1e317b2c0f1cabb414bf9f7ecc38b87893030b045e1cefb41479ccf00e72c808" "167cb309d3feab33ae33be1df5e6951f53e42b744333ddba0c93ea1177d0ee22" "2619e533e92932976ac73ba6805766f1120d3c1504c31d58e155865825f94fd4" "e2d71aa34da7d2ab67a5b34d0eb948b114f277cb97f9054248c6a33d9124f356" "6709cc0fc8af78ffd92bab8a1584fce10dde5c5bbb9d9f84d965e6582438c2a7" "477ade55cc6452f8ac3ab5c375ebbc1cb573c5f68394dc90d0343150e70539c6" "8138c703a3b42d6ecbb8421e3ea8d4a7d34cd1ae269b268be3696a6ad969427a" "98099652f97277261cad5bc951caf31b1350052a6b377fafc0ddc03edc83bbcc" "a982f6cf5eced27bf0ab5f8cae8d8f2185c88c0bbee63dd7cfcd6872960add3e" "0e239410b2206135b66f288d3090a084016f562167bfa44c3b4ce872b9344f40" "a769e14ebc1df4acfc80447e0315763ce253f3d5f9e33f3a3a2e74dcd32720c6" "cdbab9a24091a23d5e47607e1013addfaebe8aebe1a11cb4cda2482a25b8b950" "dc410b6b1639ac153a6f08adfea36db870381ecc0e5bdd547efafd19789ecaab" "d4d1ad3e5a5d264a9871f1dc9e8803facc64ca73a1cfbca503b54ec40d1abe80" "ee1f7633ef4722943f2b81d468f468d68a61522ad247fd4ce4db5b89adc4da43" "9bb04047b7b416b5aa85ef2c4c616429c3b1e3d5854e9fc77b667f0571666220" "61490a21c303eaa2d83c1312c608bd70835b36c995242f2be902faaadd2b209f" "d6091a4251cb81b5a76e14ec0114b1d2cc18ba36ab5c2193d90ef107e27cc792" "72e3a956be4ce932fff7faaafc30405729026b8257ffefce7b67ab3e62141591" "f5d5e405b860772d01ea863765be0667e099e37bac75ce283fc1fdd0cff5db27" "9484b034a292c9d05fcf8912858f7b28b7ee5f03c27cea31478f1757f2a5ca39" "ce55d5ddbc992d01562eed46400a22304cb52007f5f07b419a8a56f320fd087a" "c2e0a5e6831bf53d1e2effc19c8a86438af88e9e1cfc67575c99b263a99cf18e" "8ecff47f421a8a215c1927f8256b295aa3d9efc879db0363392375c50d742e81" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" default)))
 '(fci-rule-color "#ffe4b5")
 '(jdee-db-active-breakpoint-face-colors (cons "#100e23" "#EE82EE"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#100e23" "#4eee94"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#100e23" "#7f7f7f"))
 '(objed-cursor-color "#FF0000")
 '(package-selected-packages
   (quote
    (all-the-icons doom-modeline doom-themes py-autopep8 helm-ag helm-gtags ggtags go-mode xah-find window-numbering web-mode use-package spinner sesman seq queue powerline pkg-info paren-face org2blog markdown-mode magit highlight-parentheses helm-swoop evil-paredit elpy dired+ diminish benchmark-init)))
 '(safe-local-variable-values (quote ((encoding . UTF-8))))
 '(vc-annotate-background "#181a26")
 '(vc-annotate-color-map
   (list
    (cons 20 "#4eee94")
    (cons 40 "#89f362")
    (cons 60 "#c3f931")
    (cons 80 "#FFFF00")
    (cons 100 "#ffe100")
    (cons 120 "#ffc300")
    (cons 140 "#FFA500")
    (cons 160 "#fa982a")
    (cons 180 "#f58c55")
    (cons 200 "#f08080")
    (cons 220 "#f55555")
    (cons 240 "#fa2a2a")
    (cons 260 "#FF0000")
    (cons 280 "#df1f1f")
    (cons 300 "#bf3f3f")
    (cons 320 "#9f5f5f")
    (cons 340 "#ffe4b5")
    (cons 360 "#ffe4b5")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button))))))
