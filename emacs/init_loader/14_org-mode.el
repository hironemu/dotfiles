;; ========================================================================================
;; org-modeの設定
;; ========================================================================================
(when (require 'org-install)
  (setq org-directory "~/org/")
  (setq org-default-notes-file "note.org")
  (setq org-agenda-files (list org-default-notes-file )))
