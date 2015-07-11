;; すべてのバッファを閉じる
;; http://stackoverflow.com/questions/3417438/closing-all-other-buffers-in-emacs
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer 
	(delq (current-buffer) 
	      (remove-if-not 'buffer-file-name (buffer-list)))))

;; 古いバッファを全てrevertする
;; auto-insstall-from-url: http://www.neilvandyke.org/revbufs/revbufs.el
(require 'revbufs)
(global-set-key (kbd "C-c v") 'revbufs)

;; ファイル名がかぶった場合にバッファ名をわかりやすくする
;; http://qiita.com/icb54615/items/db1e0f7d97fcb0afe416
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; 開いているファイルのパスをコピー
;; http://d.hatena.ne.jp/CortYuming/20130802/p1
(defun copy-file-path ()
  "Show the full path file name in the minibuffer and copy to kill ring."
  (interactive)
  (when buffer-file-name
    (kill-new (file-truename buffer-file-name))
    (message (concat "Copied: " buffer-file-name))))
(global-set-key (kbd "C-c f") 'copy-file-path)

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


;; Change the keybinds to whatever you like :)
(require 'helm-swoop)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; When doing evil-search, hand the word over to helm-swoop
;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
(define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

;; Move up and down like isearch
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)

;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)

;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color nil)

;; ;; Go to the opposite side of line from the end or beginning of line
(setq helm-swoop-move-to-line-cycle t)

;; Optional face for line numbers
;; Face name is `helm-swoop-line-number-face`
(setq helm-swoop-use-line-number-face t)
