(add-hook 'csharp-mode-hook
	  '(lambda()
	     (setq c-basic-offset 4)
	     (setq indent-tabs-mode t)
	     (setq tab-width 4)
	     (c-set-offset 'substatement-open 0)
	     (c-set-offset 'case-label '+)
	     (c-set-offset 'arglist-intro '+)
	     (c-set-offset 'arglist-close 0)

             (progn
               ;; key mappings
               (define-key omnisharp-mode-map "." 'omnisharp-add-dot-and-auto-complete)
               (define-key omnisharp-mode-map "\M-/" 'omnisharp-auto-complete)
               (define-key omnisharp-mode-map "\C-c\C-g" 'omnisharp-go-to-definition)
               (define-key omnisharp-mode-map "\C-c\C-r" 'omnisharp-rename)
               (define-key omnisharp-mode-map "\C-c\C-s" 'omnisharp-helm-find-symbols)
               (define-key omnisharp-mode-map "\C-c\C-u" 'omnisharp-helm-find-usages)
               )
	     ))


(progn
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-hook 'csharp-mode-hook 'turn-on-eldoc-mode)
  (setq omnisharp-server-executable-path
        "OmniSharp/bin/Debug/OmniSharp.exe")
  )
