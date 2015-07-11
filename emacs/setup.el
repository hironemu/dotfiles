;; Emacs lisp install file
(require 'cl)

;; elispsディレクトリのパスを指す変数
(defvar my/elisp-directory)
;; init_loader(設定ファイル)のパスを指す変数
(defvar my/init-loader-directory)

;; emacsディレクトリやelisps, init_loaderディレクトリのパスを変数に設定
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name))
  (setq custom-theme-directory (concat user-emacs-directory "elisps/")))
(setq my/elisp-directory (concat user-emacs-directory "elisps/"))

;; elisps, init_loaderディレクトリがなければ作成する
(unless (file-directory-p my/elisp-directory)
  (make-directory my/elisp-directory))
(setq my/init-loader-directory (concat user-emacs-directory "init_loader"))
(unless load-file-name
  (unless (file-symlink-p my/init-loader-directory)
    (make-symbolic-link (concat default-directory "init_loader")
                        user-emacs-directory)))

;; elispsディレクトリをload-pathに追加
(add-to-list 'load-path my/elisp-directory)

;; curlコマンドがインストールされているかチェック
(dolist (cmd '("curl" "cask"))
  (unless (executable-find cmd)
    (error "Please install %s" cmd)))

;; Emacs package system
(require 'cask "/usr/local/share/emacs/site-lisp/cask.el")
(cask-initialize user-emacs-directory)

;; ELPA, Github 以外のパッケージ
(defvar my/nonelpa-packages-url
  '(
    ;; revert buffer
    "https://raw.githubusercontent.com/carljm/dotfiles/master/emacs.d/revbufs.el"
    ))

;; urlで指定されたファイルをダウンロード
(defun my/download-url (url)
  (assert (stringp url))
  (if (zerop (call-process "curl" nil nil nil "-O" url))
      (message "Success Download %s" url)
    (message "Failed Download %s" url)))

;; ELPA以外のパッケージをelispsディレクトリにインストール
(let ((default-directory my/elisp-directory))
  (dolist (url my/nonelpa-packages-url)
    (unless (file-exists-p (file-name-nondirectory url))
      (my/download-url url))))
