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
(dolist (cmd '("curl"))
  (unless (executable-find cmd)
    (error "Please install %s" cmd)))

;; Emacs package system の設定
;; package.elでインストールしたファイルはelpaディレクトリ以下にインストールされる
(require 'package)
(setq package-user-dir (concat user-emacs-directory "elpa"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; パッケージの情報を更新
(package-refresh-contents)

;; インストールするパッケージをここで指定
(defvar base-packages
  '(
    ;;;; for auto-complete
    auto-complete fuzzy popup pos-tip

    ;;;; editing utilities
    expand-region wrap-region
    undo-tree multiple-cursors smartrep
    yasnippet goto-chg
    smartparens
    ag

    ;;;; buffer utils
    elscreen yascroll

    ;;;; programming
    
    ;;flymake
    flycheck
    ;;flymake-coffee flymake-css flymake-easy flymake-haml flymake-ruby flymake-sass

    ;; coffee-script
    coffee-mode

    ;; SASS, SCSS
    sass-mode scss-mode

    ;; LESS
    less-css-mode

    ;; Haml
    haml-mode

    ;; go
    go-mode

    ;; ruby
    rvm ruby-block ruby-compilation ruby-end ruby-interpolation
    ruby-test-mode ruby-tools inf-ruby ruby-refactor yari
    
    ;; emacs-lisp
    elisp-slime-nav thingopt

    ;; common utility
    quickrun s

    ;;;; markup language
    htmlize web-mode yaml-mode emmet-mode
    markdown-mode markdown-mode+

    ;; helm
    helm

    ;; git
    magit git-gutter git-commit-mode git-rebase-mode

    ;; directory operation
    direx 
    pkg-info projectile

    ;; basic
    init-loader solarized-theme zenburn-theme exec-path-from-shell anzu redo+ maxframe
    
    ))
(defvar sub-packages
  '(
    ;; auto-complete
    go-autocomplete

    ;; popwin
    ;import-popwin

    ;; go
    go-eldoc

    ;; helm
    helm-descbinds helm-ag helm-c-moccur helm-projectile
    ))

;; ELPA外のパッケージ
(defvar my/nonelpa-packages-url
  '(
    "https://raw.github.com/emacsmirror/auto-highlight-symbol/master/auto-highlight-symbol.el"
    "http://www.neilvandyke.org/revbufs/revbufs.el"
    ))

;; ベースパッケージのインストール
(dolist (package base-packages)
  (when (or (not (package-installed-p package))
            (memq package '(cperl-mode ruby-mode)))
    (package-install package)))

;; サブパッケージのインストール
(dolist (package sub-packages)
  (unless (package-installed-p package)
    (package-install package)))

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
