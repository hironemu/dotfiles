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

;; Emacsのツールバーを非表示にする
(tool-bar-mode 0)

;; タイトルバーにフルパスを表示
(setq frame-title-format "%f")

;; C-hでバックスペース
(global-set-key "\C-h" 'delete-backward-char)

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

;; Emacs終了時にバッファの状態を保存する
(desktop-save-mode t)

;; Windowの透過
(set-frame-parameter nil 'alpha 100)

;; 起動時にWindowを最大化
(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

;; Elscreen
;; TODO なんか動かない
;; (load "elscreen" "ElScreen" t) ;; TODO 自動起動しない
;; (define-key global-map (kbd "M-t") 'elscreen-next)


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
 '(custom-safe-themes (quote ("6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" "e9a1226ffed627ec58294d77c62aa9561ec5f42309a1f7a2423c6227e34e3581" "211bb9b24001d066a646809727efb9c9a2665c270c753aa125bace5e899cb523" default))))
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
  (ac-config-default))

;; 検索
(require 'color-moccur)
(require 'moccur-edit)

;; ========================================================================================
;; Helmの設定
;; ========================================================================================
(helm-mode 1)
(global-set-key (kbd "C-;") 'helm-mini)


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

;; ruby-blockの設定
;; endに対するハイライト
(when (require 'ruby-block nil t)
  (setq ruby-block-highlight-toggle t))

(require 'ruby-tools)
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
  )

(add-hook 'ruby-mode-hook
	  'ruby-mode-hooks)


;; RVMの設定
;; $ rvm use defaultとした時と同じRubyを使用する
(rvm-use-default)

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

