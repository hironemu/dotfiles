;; ========================================================================================
;; Helmの設定
;; ========================================================================================
(require 'helm-config)
(helm-descbinds-mode)
(global-set-key (kbd "C-;") 'helm-mini)

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-:") 'helm-resume)
(global-set-key (kbd "M-s") 'helm-occur)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c s") 'helm-do-grep)
(global-set-key (kbd "C-c h") 'helm-descbinds)
(global-set-key (kbd "C-c ;") 'helm-resume)


