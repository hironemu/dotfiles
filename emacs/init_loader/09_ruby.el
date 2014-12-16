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

;; rspec-simple
;; https://github.com/code-mancers/rspec-simple
(add-to-list 'load-path "~/.emacs.d/elisps/rspec-simple")
(require 'rspec-simple)

;; ruby-mode-hook用関数定義
(defun ruby-mode-hooks ()
  ;; C-;がhelm-miniの起動とかぶるのでC-.に変更
  (define-key ruby-tools-mode-map (kbd "C-;") nil)
  (define-key ruby-tools-mode-map (kbd "C-.") 'ruby-tools-clear-string)
  
  ;; 引数のリストを改行して揃えるときインデントが深くなりすぎるのを防ぐ
  (setq ruby-deep-indent-paren nil)
  
  ;; ruby-modeで起動するファイル
  ;; http://stackoverflow.com/questions/11027783/how-can-i-cons-a-list-of-pairs-on-to-auto-mode-alist
  (let* ((ruby-files '(".rake" ".thor" "Gemfile" "Rakefile" "Crushfile" "Capfile" "Guardfile"))
  	 (ruby-regexp (concat (regexp-opt ruby-files t) "\\'")))
    (add-to-list 'auto-mode-alist (cons ruby-regexp 'ruby-mode)))

  ;; マジックコメントを自動で入れない
  (setq ruby-insert-encoding-magic-comment nil)

  ;; rspec-simple config
  (local-set-key (kbd "C-c o") 'rspec-display-file-outline)
  (local-set-key (kbd "C-c i") 'rspec-compile-on-line)
  (local-set-key (kbd "C-c r") 'rspec-compile-file)
  (local-set-key (kbd "s-t") 'rspec-find-related-file)
  
  )
(add-hook 'ruby-mode-hook
	  'ruby-mode-hooks)
