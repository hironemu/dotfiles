;; ダイアログボックスを使わない
(setq use-dialog-box nil)

;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)

;; Emacsのツールバーを非表示にする
(tool-bar-mode 0)

;; スクロールバーを消す
(scroll-bar-mode -1)
;; yascroll
;; http://d.hatena.ne.jp/m2ym/20110401/1301617991
(global-yascroll-bar-mode 1)

;; タイトルバーにフルパスを表示
(setq frame-title-format "%f")

;; C-hでバックスペース
(global-set-key "\C-h" 'delete-backward-char)

;; C-jで改行+インデント
(global-set-key "\C-j" 'newline-and-indent)

;; 選択した部分をDELやBSで削除できるようにする
(delete-selection-mode t)

;; 指定した行にジャンプ
(global-set-key (kbd "M-g") 'goto-line)

;; 折り返しトグルコマンド
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)

;; コードの整形
(define-key global-map (kbd "C-c a") 'align-regexp)

;; Emacs終了時にバッファの状態を保存する
(desktop-save-mode t)

;; Windowの透過
;; (set-frame-parameter nil 'alpha 95)

;; 起動時にWindowを最大化
;(toggle-frame-maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Emacs上にファイルをドラッグ＆ドロップして開く
(define-key global-map [ns-drag-file] 'ns-find-file)

;; ドラッグ＆ドロップで新しくウィンドウを開かない
(setq ns-pop-up-frames nil)

;; redo+
;; http://qiita.com/icb54615/items/cbcf021eec77f546c7b6
(when (require 'redo+ nil t)
  (global-set-key (kbd "C-?") 'redo)
  (setq undo-no-redo t) ; 過去のundoがredoされないようにする
  (setq undo-limit 600000)
  (setq undo-strong-limit 900000))

;; term を使うときにm4という文字が先頭に入る場合の対処方法
;; % mkdir -p ~/.terminfo/65/
;; % cp /usr/share/terminfo/65/eterm ~/.terminfo/65/eterm-color
;; (setq system-uses-terminfo nil) ; ＜これが効かない
(setenv "LANG" "ja_JP.UTF-8")
(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;; Ricty fontの使用と設定
;; http://blog.nyarla.net/2011/10/28/1
(let* ((size 15)
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

;; muliple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; カーソル行ハイライト
;; http://stackoverflow.com/questions/4495406/hl-line-mode-emacs-color-change
(defadvice hl-line-mode (after
			 dino-advise-hl-line-mode
			 activate compile)
  (set-face-background hl-line-face "gray13"))
(global-hl-line-mode)

;; expand-region
(require 'expand-region)
(global-set-key (kbd "M-2") 'er/expand-region)
(global-set-key (kbd "M-3") 'er/contract-region)

;; 括弧などのペアになる文字列を扱う
;; https://github.com/Fuco1/smartparens
(require 'smartparens-config)
(smartparens-global-mode 1)

;; highlight-symbol
(setq highlight-symbol-idle-delay 0.3)
(add-hook 'ruby-mode-hook 'highlight-symbol-mode)
