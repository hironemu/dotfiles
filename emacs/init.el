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
