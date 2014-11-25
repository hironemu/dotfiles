;; The Silver Searcher (ag)
;; $ requirebrew instal the_silver_searcher
;; https://github.com/Wilfred/ag.el
;; 除外したいものは.agignoreに記述する
(when (and (executable-find "ag")
	   (require 'ag nil t)))

;; helm-ag use projectile root
(defun helm-ag-projectile ()
  (interactive)
  (helm-ag (projectile-project-root)))
