;; ========================================================================================
;; goの設定
;; ========================================================================================
(when (and (require 'go-mode nil t) (executable-find "go"))
  ;; GOROOT, GOPATH環境変数を読み込む
  (let ((envs '("GOROOT" "GOPATH")))
    (exec-path-from-shell-copy-envs envs))
  ;; go-autocompleteの読み込み
  (add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/nsf/gocode/emacs"))
  (require 'go-autocomplete)
  (require 'auto-complete-config)
  (require 'go-flycheck)
  ;;(add-hook 'go-mode-hook 'flycheck-mode)

  (defun go-mode-hooks ()
    "hooks for go-mode"
    ;; タブサイズ
    (setq tab-width 4)
    ;; go-eldocの設定
    (go-eldoc-setup)
    )
  (add-hook 'go-mode-hook 'go-mode-hooks)
  (add-hook 'go-mode-hook 'flycheck-mode)
  )
