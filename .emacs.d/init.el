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
(set-frame-parameter nil 'alpha 93)

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
(let* ((size 16)
       (asciifont "Ricty") ; ASCII fonts
       (jpfont "Ricty") ; Japanese fonts
       (h (* size 10))
       (fontspec (font-spec :family asciifont))
       (jp-fontspec (font-spec :family jpfont)))
  (set-face-attribute 'default nil :family asciifont :height h)
  (set-fontset-font nil 'japanese-jisx0213.2004-1 jp-fontspec)
  (set-fontset-font nil 'japanese-jisx0213-2 jp-fontspec)
  (set-fontset-font nil 'katakana-jisx0201 jp-fontspec)
  (set-fontset-font nil '(#x0080 . #x024F) fontspec) 
  (set-fontset-font nil '(#x0370 . #x03FF) fontspec))

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
 '(ansi-color-names-vector ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-enabled-themes (quote (tango-2)))
 '(custom-safe-themes (quote ("0322f45e2d84bf7c29937355ea600a852f999461f8262243d71aeb9ce99b782f" "cfcc3ed8e51b9f1c2e99166349d5cb1b4ce392b167db73bd83c5ccb706443a8d" "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" "e9a1226ffed627ec58294d77c62aa9561ec5f42309a1f7a2423c6227e34e3581" "211bb9b24001d066a646809727efb9c9a2665c270c753aa125bace5e899cb523" default))))
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
  (setq ac-delay 1))

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
  (add-to-list 'auto-mode-alist
	       '("\\.rake$" . ruby-mode))
  ;; ソースコードの折りたたみ
  (hs-minor-mode 1)
  (define-key hs-minor-mode-map (kbd "C-c ,l") 'hs-hide-level)
  (define-key hs-minor-mode-map (kbd "C-c ,s") 'hs-show-all)
  (define-key ruby-mode-map (kbd "M-1") 'helm-rdefs)
  )

(add-hook 'ruby-mode-hook
	  'ruby-mode-hooks)

;; (require 'hideshow-org)

;; (add-to-list 'hs-special-modes-alist
;; 	     '(ruby-mode
;; 	       "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
;; 	       (lambda (arg) (ruby-end-of-block)) nil))

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
	     (flymake-haml-load)
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
  (linum-mode 1))
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
  (set-face-background hl-line-face "gray15"))
(global-hl-line-mode)

;; 古いバッファを全てrevertする
;; auto-insstall-from-url: http://www.neilvandyke.org/revbufs/revbufs.el
(require 'revbufs)
(global-set-key (kbd "C-c v") 'revbufs)

;; popwin
;; https://github.com/m2ym/popwin-el
(when (require 'popwin nil t)
  (setq display-buffer-function 'popwin:display-buffer)
  (push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
  (push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config))

;; expand-region
(require 'expand-region)
(global-set-key (kbd "M-2") 'er/expand-region)
(global-set-key (kbd "M-3") 'er/contract-region)

;; 括弧などのペアになる文字列を扱う
;; https://github.com/Fuco1/smartparens
(require 'smartparens-config)
(smartparens-global-mode 1)
