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
