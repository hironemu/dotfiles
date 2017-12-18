;; flycheck
(setq flycheck-display-errors-delay 0.3) ; エラーメッセージ表示までの時間(デフォルト0.9秒)
(add-hook 'c-mode-common-hook 'flycheck-mode)
(add-hook 'ruby-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'flycheck-mode)
