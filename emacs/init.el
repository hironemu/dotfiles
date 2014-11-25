;; for lancher
(unless load-file-name
  (cd (getenv "HOME")))

;; Add load path of emacs lisps
(add-to-list 'load-path (concat user-emacs-directory "elisps"))

;; Emacs package system
(require 'cask "/usr/local/share/emacs/site-lisp/cask.el")
(cask-initialize)

;; load environment variables
(let ((envs '("PATH" "GOROOT" "GOPATH")))
  (exec-path-from-shell-copy-envs envs))

;; init-loader
(require 'init-loader)
(init-loader-load (concat user-emacs-directory "init_loader"))
(put 'upcase-region 'disabled nil)
