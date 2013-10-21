;; MELPAリポジトリを追加
;; http://batsov.com/articles/2012/04/06/melpa-homebrew-emacs-edition/
;; http://sakito.jp/emacs/emacs24.html
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)


;; 独自にインストールしたパッケージのロード
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))
(add-to-load-path "elisp")

;; Pathの設定
(dolist (dir (list
              "/sbin"
              "/usr/sbin"
              "/bin"
              "/usr/bin"
              "/opt/local/bin"
              "/sw/bin"
              "/usr/local/bin"
              (expand-file-name "~/bin")
              (expand-file-name "~/.emacs.d/bin")
              ))
  ;; PATH と exec-path に同じ物を追加します
  (when (and (file-exists-p dir) (not (member dir exec-path)))
    (setenv "PATH" (concat dir ":" (getenv "PATH")))
    (setq exec-path (append (list dir) exec-path))))

;;auto-installの設定
(when(require 'auto-install nil t)
  ;;インストールディレクトリを設定する初期値は~/.emacs.d/auto-install/
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;;EmacsWikiに登録されているelispの名前を取得する
  ;(auto-install-update-emacswiki-package-name t)
  ;;必要であればプロキシの設定を行う
  ;;(setq url-proxy-services '(("http" . "localhost:8339")))
  ;;install-elispの関数を利用可能にする
  (auto-install-compatibility-setup))

;; バックアップファイルを作らない
;; (setq make-backup-files nil)
;; ;; 自動保存しない
;; (setq auto-save-default nil)
;; (setq backup-inhibited t)

;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)

;; Emacsのツールバーを非表示にする
(tool-bar-mode 0)

;; スクロールバーを消す
(toggle-scroll-bar nil)

;; タイトルバーにフルパスを表示
(setq frame-title-format "%f")

;; C-hでバックスペース
(global-set-key "\C-h" 'delete-backward-char)

;; 選択した部分をDELやBSで削除できるようにする
(delete-selection-mode t)

;; C-t に other-window
;; C-S-t でother-windowを戻る
;; http://d.hatena.ne.jp/rubikitch/20100210/emacs
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(defun other-window-back ()
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-t") 'other-window-or-split)
(global-set-key (kbd "C-S-T") 'other-window-back)

;; 折り返しトグルコマンド
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)

;; コードの整形
(define-key global-map (kbd "C-c a") 'align-regexp)

;; Emacs終了時にバッファの状態を保存する
(desktop-save-mode t)

;; Windowの透過
;(set-frame-parameter nil 'alpha 93)

;; 起動時にWindowを最大化
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

;; Emacs上にファイルをドラッグ＆ドロップして開く
(define-key global-map [ns-drag-file] 'ns-find-file)

;; ドラッグ＆ドロップで新しくウィンドウを開かない
(setq ns-pop-up-frames nil)

;; すべてのバッファを閉じる
;; http://stackoverflow.com/questions/3417438/closing-all-other-buffers-in-emacs
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer 
	(delq (current-buffer) 
	      (remove-if-not 'buffer-file-name (buffer-list)))))

;; ファイル名がかぶった場合にバッファ名をわかりやすくする
;; http://qiita.com/icb54615/items/db1e0f7d97fcb0afe416
(require 'uniquify)
(setq
 uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Elscreen
;; http://d.hatena.ne.jp/sky-y/20120830/1346333199
(when (>= emacs-major-version 24)
   (elscreen-start))


;; term を使うときにm4という文字が先頭に入る場合の対処方法
;; % mkdir -p ~/.terminfo/65/
;; % cp /usr/share/terminfo/65/eterm ~/.terminfo/65/eterm-color
;; (setq system-uses-terminfo nil) ; ＜これが効かない
(setenv "LANG" "ja_JP.UTF-8")
(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;; term-modeの幅をかえる。term-window-widthを上書き。
;; http://stackoverflow.com/questions/11036739/fixed-width-in-emacs-term-mode

;; C-c tでmulti-term起動
(global-set-key (kbd "C-c t") 'multi-term)
(defun term-window-width () 80) ;; TODO これ毎回評価しないとダメなんだけどなんで。
;; (require 'multi-term)
(add-hook 'term-mode-hook
          '(lambda ()
	     (define-key term-raw-map (kbd "C-h") 'term-send-backspace)
	     (define-key term-raw-map (kbd "C-y") 'term-paste)
	     (define-key term-raw-map (kbd "C-t") 'other-window)
	     (define-key term-raw-map (kbd "C-T") 'other-window)
	     (setq truncate-lines t)
	     ))



;; テーマファイルの場所
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; Ricty fontの使用と設定
;; http://blog.nyarla.net/2011/10/28/1
;; (let* ((size 16)
;;        (asciifont "Ricty") ; ASCII fonts
;;        (jpfont "Ricty") ; Japanese fonts
;;        (h (* size 10))
;;        (fontspec (font-spec :family asciifont))
;;        (jp-fontspec (font-spec :family jpfont)))
;;   (set-face-attribute 'default nil :family asciifont :height h)
;;   (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
;;   (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
;;   (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)
;;   (set-fontset-font nil '(#x0080 . #x024F) fontspec) 
;;   (set-fontset-font nil '(#x0370 . #x03FF) fontspec))
(let* ((size 16)
       (asciifont "Ricty") ; ASCII fonts
       (jpfont "Ricty") ; Japanese fonts
       (h (* size 10))
       (fontspec (font-spec :family asciifont))
       (jp-fontspec (font-spec :family jpfont)))
  (set-face-attribute 'default nil :family asciifont :height h)
  ;; (set-face-bold-p 'bold nil)
  (set-fontset-font t 'japanese-jisx0213.2004-1 jp-fontspec)
  (set-fontset-font t 'japanese-jisx0213-2 jp-fontspec)
  (set-fontset-font t 'japanese-jisx0213-1 jp-fontspec)
  (set-fontset-font t 'japanese-jisx0212 jp-fontspec)
  (set-fontset-font t 'japanese-jisx0208 jp-fontspec)
  (set-fontset-font t 'katakana-jisx0201 jp-fontspec)
  (set-fontset-font t '(#x0080 . #x024F) fontspec) 
  (set-fontset-font t '(#x0370 . #x03FF) fontspec))

(dolist (elt '(("^-apple-hiragino.*" . 1.2)
	       (".*osaka-bold.*" . 1.2)
	       (".*osaka-medium.*" . 1.2)
	       (".*courier-bold-.*-mac-roman" . 1.0)
	       (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
	       (".*monaco-bold-.*-mac-roman" . 0.9)))
  (add-to-list 'face-font-rescale-alist elt))

;; フォントサイズを一時的に変更する
;; http://emacs-fu.blogspot.jp/2008/12/zooming-inout.html
(defun djcb-zoom (n)
  "with positive N, increase the font size, otherwise decrease it"
  (set-face-attribute 'default (selected-frame) :height 
    (+ (face-attribute 'default :height) (* (if (> n 0) 1 -1) 10)))) 
(global-set-key (kbd "C-+")      '(lambda nil (interactive) (djcb-zoom 1)))
(global-set-key [C-kp-add]       '(lambda nil (interactive) (djcb-zoom 1)))
(global-set-key (kbd "C--")      '(lambda nil (interactive) (djcb-zoom -1)))
(global-set-key [C-kp-subtract]  '(lambda nil (interactive) (djcb-zoom -1)))

;; custom-theme-load-pathで指定したディレクトリにテーマファイルを配置
;; M-x customize-themesでテーマを選択
;; 設定を保存するボタンを押すと自動で以下の設定が保存される
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#002b36" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes (quote ("e9a1226ffed627ec58294d77c62aa9561ec5f42309a1f7a2423c6227e34e3581" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "211bb9b24001d066a646809727efb9c9a2665c270c753aa125bace5e899cb523" "1d1622e8bc2292dab58d7ba452cef0ac81463dcf021f3f5a65afb0d551c1d746" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" default)))
 '(fci-rule-color "#073642")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-tail-colors (quote (("#073642" . 0) ("#546E00" . 20) ("#00736F" . 30) ("#00629D" . 50) ("#7B6000" . 60) ("#8B2C02" . 70) ("#93115C" . 85) ("#073642" . 100))))
 '(syslog-debug-face (quote ((t :background unspecified :foreground "#2aa198" :weight bold))))
 '(syslog-error-face (quote ((t :background unspecified :foreground "#dc322f" :weight bold))))
 '(syslog-hour-face (quote ((t :background unspecified :foreground "#859900"))))
 '(syslog-info-face (quote ((t :background unspecified :foreground "#268bd2" :weight bold))))
 '(syslog-ip-face (quote ((t :background unspecified :foreground "#b58900"))))
 '(syslog-su-face (quote ((t :background unspecified :foreground "#d33682"))))
 '(syslog-warn-face (quote ((t :background unspecified :foreground "#cb4b16" :weight bold))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#dc322f") (40 . "#CF4F1F") (60 . "#C26C0F") (80 . "#b58900") (100 . "#AB8C00") (120 . "#A18F00") (140 . "#989200") (160 . "#8E9500") (180 . "#859900") (200 . "#729A1E") (220 . "#609C3C") (240 . "#4E9D5B") (260 . "#3C9F79") (280 . "#2aa198") (300 . "#299BA6") (320 . "#2896B5") (340 . "#2790C3") (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list (quote (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; auto-completeの設定
;; 入力補完を行う
(when (require 'auto-complete-config nil t)
  ;; M-/で候補を表示
  (define-key ac-mode-map (kbd "M-/") 'auto-complete)
  ;; C-nで前の候補へ移動
  (define-key ac-complete-mode-map "\C-n" 'ac-next)
  ;; C-pで次の候補へ移動
  (define-key ac-complete-mode-map "\C-p" 'ac-previous)
  (ac-config-default)
  ;; 補完開始までの遅延時間（秒数）
  (setq ac-delay 0.3))

;; 検索
(require 'color-moccur)
(require 'moccur-edit)

;; moccur-grep時のデフォルトのファイルマスク
(setq-default moccur-grep-default-mask ".rb")

;; (require 'highlight-symbol)
;; ;; (global-set-key [(control f3)] 'highlight-symbol-at-point)
;; (global-set-key (kbd "C-.") 'highlight-symbol-at-point)
;; (global-set-key (kbd "C-}") 'highlight-symbol-next)
;; (global-set-key (kbd "C-{") 'highlight-symbol-prev)
;; (custom-set-variables '(highlight-symbol-foreground-color "white"))

(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)

;; redo+
;; http://qiita.com/icb54615/items/cbcf021eec77f546c7b6
(when (require 'redo+ nil t)
  (global-set-key (kbd "C-?") 'redo)
  (setq undo-no-redo t) ; 過去のundoがredoされないようにする
  (setq undo-limit 600000)
  (setq undo-strong-limit 900000))


;; ========================================================================================
;; Helmの設定
;; ========================================================================================
(helm-mode 1)
(global-set-key (kbd "C-;") 'helm-mini)
;; 色々なhelmのコマンドとか以下のサイトを参考に。
;; http://sleepboy-zzz.blogspot.jp/2012/09/anythinghelm.html
(require 'helm-descbinds)
(setq helm-idle-delay 0.3
      helm-input-idle-delay 0.1
      helm-candidate-number-limit 200)
;; HelmのFile listでパスを表示する
;; http://mikio.github.io/article/2013/06/14_helm.html
(setq helm-ff-transformer-show-only-basename nil)


(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-:") 'helm-resume)
(global-set-key (kbd "M-s") 'helm-occur)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c s") 'helm-do-grep)
(global-set-key (kbd "C-c h") 'helm-descbinds)

;; ========================================================================================
;; = magitの設定
;; ========================================================================================
;; C-c mでmagit-statusを起動
(global-set-key (kbd "C-c m") 'magit-status)

;; change magit diff colors
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-section-title "SteelBlue1")
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")
     (when (not window-system)
       (set-face-background 'magit-item-highlight "black"))))

;; ========================================================================================
;; Projectileの設定
;; ========================================================================================
;; https://github.com/bbatsov/projectile
(when (require 'projectile nil t)
  (projectile-global-mode)
  (global-set-key (kbd "C-c h") 'helm-projectile)
  )

;; ========================================================================================
;; Rubyの設定
;; ========================================================================================

;; RVMの設定
;; $ rvm use defaultとした時と同じRubyを使用する
(require 'rvm)
(rvm-use-default)

;; ruby-blockの設定
;; endに対するハイライト
(when (require 'ruby-block nil t)
  (setq ruby-block-highlight-toggle t))

(require 'ruby-tools)
(require 'helm-rdefs)

;; ruby-mode-hook用関数定義
(defun ruby-mode-hooks ()
  ;; (inf-ruby-keys)
  ;; (ruby-tools-mode t)
  (ruby-end-mode t)
  ;; C-;がhelm-miniの起動とかぶるのでC-.に変更
  (define-key ruby-tools-mode-map (kbd "C-;") nil)
  (define-key ruby-tools-mode-map (kbd "C-.") 'ruby-tools-clear-string)

  ;; Returnキーを押した時改行と同時にインデントも行う
  (local-set-key "\r" 'newline-and-indent)

  ;; 行番号の表示
  (linum-mode 1)
  ;; 引数のリストを改行して揃えるときインデントが深くなりすぎるのを防ぐ
  (setq ruby-deep-indent-paren nil)
  ;; Rubyのシンタックスチェックを行う
  (flymake-ruby-load)
  ;; ruby-modeで起動するファイル
  ;; http://stackoverflow.com/questions/11027783/how-can-i-cons-a-list-of-pairs-on-to-auto-mode-alist
  (let* ((ruby-files '(".rake" ".thor" "Gemfile" "Rakefile" "Crushfile" "Capfile" "Guardfile"))
	 (ruby-regexp (concat (regexp-opt ruby-files t) "\\'")))
    (add-to-list 'auto-mode-alist (cons ruby-regexp 'enh-ruby-mode)))
  ;; ソースコードの折りたたみ
  (hs-minor-mode 1)
  (define-key hs-minor-mode-map (kbd "C-c ,l") 'hs-hide-level)
  (define-key hs-minor-mode-map (kbd "C-c ,s") 'hs-show-all)
  (define-key ruby-mode-map (kbd "M-1") 'helm-rdefs)
  )
(add-hook 'ruby-mode-hook
	  'ruby-mode-hooks)
;; (add-hook 'enh-ruby-mode-hook
;; 	  'ruby-mode-hooks)

;; (require 'hideshow-org)

;; ソースコードの折りたたみ
;; http://yoosee.net/d/archives/2007/01/30/002.html
(let ((ruby-mode-hs-info
       '(ruby-mode
	 "class\\|module\\|def\\|if\\|unless\\|case\\|while\\|until\\|for\\|begin\\|do"
	 "end"
	 "#"
	 ruby-move-to-block
	 nil)))
  (if (not (member ruby-mode-hs-info hs-special-modes-alist))
      (setq hs-special-modes-alist
            (cons ruby-mode-hs-info hs-special-modes-alist))))


;; Emacs 24.1にしてからhaml-modeのシンタックスハイライトがおかしいので以下を使う
;; cd .emacs.d/elisp && wget https://raw.github.com/ealden/haml-mode/master/haml-mode.el
(require 'haml-mode)
(require 'sass-mode)

;; haml-modeの設定
;; hamlコマンドが必要（RVMを使っている場合はrvm.elが必要)
(add-hook 'haml-mode-hook
	  '(lambda ()
	     (setq indent-tabs-mode nil)
	     (define-key haml-mode-map "\C-m" 'newline-and-indent)
	     ;; (flymake-haml-load)
	     ))

;; scss-modeの設定
(add-hook 'scss-mode-hook
	  '(lambda ()
	     ;; SCSSの自動コンパイルをOFFにする
	     ;; 手動でコンパイルするときはC-c C-c
	     (setq scss-compile-at-save nil)))

;; 「Symbols function definition is void: first」というエラーがscssのコンパイルで出たので
;; clを使うように設定する
(require 'cl)

;; coffee-modeの設定
(defun coffee-mode-hooks ()
  ;; タブサイズを2に設定
  (set (make-local-variable 'tab-width) 2)
  (set (make-local-variable 'coffee-tab-width) 2))
(add-hook 'coffee-mode-hook
	  'coffee-mode-hooks)

;; Less modeの設定
(add-hook 'css-mode-hook
	  (lambda ()
	    (setq css-indent-offset 2)
	    (setq indent-tabs-mode nil)
	    ))

;; Markdown mode
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md$" . markdown-mode) auto-mode-alist));)
(setq markdown-css-path
      "http://netdna.bootstrapcdn.com/twitter-bootstrap/2.2.1/css/bootstrap-combined.min.css")

;; web-mode
(require 'web-mode)
;;; 適用する拡張子
(add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.php$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))

;;; インデント数
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-html-offset   2)
  (setq web-mode-css-offset    2)
  (setq web-mode-script-offset 2)
  (setq web-mode-php-offset    2)
  (setq web-mode-java-offset   2)
  (setq web-mode-asp-offset    2)
  ;(linum-mode 1)
  )
(add-hook 'web-mode-hook 'web-mode-hook)

(setq js-indent-level 2)

;; muliple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; (require 'inline-string-rectangle)
;; (global-set-key (kbd "C-x r t") 'inline-string-rectangle)

;; (require 'mark-more-like-this)
;; (global-set-key (kbd "C-<") 'mark-previous-like-this)
;; (global-set-key (kbd "C->") 'mark-next-like-this)
;; (global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)
;; (global-set-key (kbd "C-*") 'mark-all-like-this)

;; (add-hook 'sgml-mode-hook
;;           (lambda ()
;;             (require 'rename-sgml-tag)
;;             (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)))

;; (defun my-mark-current-word (&optional arg allow-extend)
;;   "Put point at beginning of current word, set mark at end."
;;   (interactive "p\np")
;;   (setq arg (if arg arg 1))
;;   (if (and allow-extend
;; 	   (or (and (eq last-command this-command) (mark t))
;; 	       (region-active-p)))
;;       (set-mark
;;        (save-excursion
;; 	 (when (< (mark) (point))
;; 	   (setq arg (- arg)))
;; 	 (goto-char (mark))
;; 	 (forward-word arg)
;; 	 (point)))
;;     (let ((wbounds (bounds-of-thing-at-point 'word)))
;;       (unless (consp wbounds)
;; 	(error "No word at point"))
;;       (if (>= arg 0)
;; 	  (goto-char (car wbounds))
;; 	(goto-char (cdr wbounds)))
;;       (push-mark (save-excursion
;; 		   (forward-word arg)
;; 		   (point)))
;;       (activate-mark))))


;; ;; カーソル位置の単語を選択する
;; ;; http://ergoemacs.org/emacs/elisp_examples.html
;; (transient-mark-mode 1)
;; (defun select-current-word ()
;; "Select the word under cursor.
;; “word” here is considered any alphanumeric sequence with “_” or “-”."
;;  (interactive)
;;  (let (pt)
;;    (skip-chars-backward "-_A-Za-z0-9")
;;    (setq pt (point))
;;    (skip-chars-forward "-_A-Za-z0-9")
;;    (set-mark pt)
;;  ))
;; (global-set-key (kbd "M-2") 'select-current-word)

;; カーソル行ハイライト
;; http://stackoverflow.com/questions/4495406/hl-line-mode-emacs-color-change
(defadvice hl-line-mode (after
			 dino-advise-hl-line-mode
			 activate compile)
  (set-face-background hl-line-face "gray13"))
(global-hl-line-mode)

;; ;; カーソルの色
;; (set-cursor-color "#D8FAD4")

;; regionの背景色
;; (set-face-background 'region "#333366")

;; 古いバッファを全てrevertする
;; auto-insstall-from-url: http://www.neilvandyke.org/revbufs/revbufs.el
(require 'revbufs)
(global-set-key (kbd "C-c v") 'revbufs)

;; ;; popwin
;; ;; https://github.com/m2ym/popwin-el
;; (when (require 'popwin nil t)
;;   (setq display-buffer-function 'popwin:display-buffer)
;;   (push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
;;   (push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config)
;;   (push '("^\*magit:.+\*$" :regexp t) popwin:special-display-config)
;;   ;; popwinを使っていると max-specpdl-size エラーと max-lisp-eval-depth エラーが出るので以下を追加
;;   ;; http://d.hatena.ne.jp/a666666/20100221/1266695355
;;   (setq max-specpdl-size 6000)
;;   (setq max-lisp-eval-depth 1000)
;;   )

;; expand-region
(require 'expand-region)
(global-set-key (kbd "M-2") 'er/expand-region)
(global-set-key (kbd "M-3") 'er/contract-region)

;; 括弧などのペアになる文字列を扱う
;; https://github.com/Fuco1/smartparens
(require 'smartparens-config)
(smartparens-global-mode 1)

;; ;;モードラインをかこよく
;; ;; http://github.com/milkypostman/powerline/
(when (require 'powerline nil t)
  ;; Solarized themeに合わせた色に設定
  (set-face-background 'powerline-active1 "#00629D")
  (set-face-foreground 'powerline-active1 "#fdf6e3")
  (set-face-background 'powerline-active2 "#268bd2")
  (set-face-foreground 'powerline-active2 "#fdf6e3")
  (powerline-default-theme))

;; ローマ字のまま日本語検索 migemo
;; $ brew install cmigemo
;; migemo.el > https://github.com/emacs-jp/migemo
(when (require 'migemo)
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs" "-i" "\g"))
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init))

;; The Silver Searcher (ag)
;; $ brew instal the_silver_searcher
;; https://github.com/Wilfred/ag.el
;; 除外したいものは.agignoreに記述する
(when (require 'ag))
