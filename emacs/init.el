;; for lancher
(unless load-file-name
  (cd (getenv "HOME")))

;; Add load path of emacs lisps
(add-to-list 'load-path (concat user-emacs-directory "elisps"))
(add-to-list 'load-path (concat user-emacs-directory "my_elisps"))

;; Emacs package system
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; load environment variables
(let ((envs '("PATH" "GOROOT" "GOPATH")))
  (exec-path-from-shell-copy-envs envs))

;; init-loader
(require 'init-loader)
(init-loader-load (concat user-emacs-directory "init_loader"))
(put 'upcase-region 'disabled nil)

;; Theme Settings
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
