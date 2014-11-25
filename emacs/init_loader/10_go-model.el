;; ========================================================================================
;; goの設定
;; ========================================================================================
(when (and (require 'go-mode nil t) (executable-find "go"))
  ;; GOROOT, GOPATH環境変数を読み込む
  ;; (let ((envs '("GOROOT" "GOPATH")))
  ;;   (exec-path-from-shell-copy-envs envs))
  ;; go-autocompleteの読み込み
  (eval-after-load "go-mode"
    '(progn
       (require 'go-autocomplete)))
  ;; goflaymakeの読み込み
  ;;(add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/dougm/goflymake"))
  ;;(require 'go-flymake)
  (add-hook 'go-mode-hook 'flycheck-mode)

  ;; go-eldocの設定
  ;; https://github.com/syohex/emacs-go-eldoc
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  ;; (set-face-attribute 'eldoc-highlight-function-argument nil
  ;; 		      :underline t :foreground "green"
  ;; 		      :weight 'bold)
  )
