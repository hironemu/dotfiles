;;; helm-rdefs.el

;; Copyright (C) 2011-2012 mori_dev

;; Author: hironemu <hironemu@gmail.com>
;; Keywords: helm, ruby
;; Prefix: ar:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Installation:

;; install requires libraries:
;; `ruby-mode.el'
;; `helm.el'
;; `helm-config.el'
;; `rdefs'                 http://i.loveruby.net/svn/public/bin/trunk/rdefs.rb

;;; Setting Sample
;;
;; mv rdefs.rb ~/bin/rdefs
;;
;; (require 'helm-rdefs)
;; (add-hook 'ruby-mode-hook
;;           (lambda ()
;;             (define-key ruby-mode-map (kbd "C-@") 'helm-rdefs)))


(require 'cl)
(require 'helm-config)

(defvar ar:recenter-height 10)
(defvar ar:command (executable-find "rdefs"))
(defvar ar:buffer "*rdefs*")
(defvar ar:current-line-overlay (make-overlay (point) (point)))
(defvar ar:enable-auto-look-flag t)
(defvar ar:push-mark-flag t)


(defun ar:substring-line-number (s)
  (when (string-match "\\([0-9]+\\):" s)
    (match-string 1 s)))

(defmacro ar:aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro* ar:awhen (test-form &body body)
  `(ar:aif ,test-form
        (progn ,@body)))

(defvar helm-c-source-rdefs
  '((name . "rdefs")
    (init . helm-c-rdefs-init)
    (candidates-in-buffer)
    (candidate-number-limit . 9999)
    (action . helm-c-rdefs-action)
    (persistent-action . ar:persistent-action)
    (cleanup . ar:clean-up)))

(defun helm-rdefs ()
  (interactive)
  (let ((helm-display-function 'ar:display-buffer)
         (helm-buffer ar:buffer)
         (helm-map ar:helm-map))
    (letf (((symbol-function 'helm-create-helm-buffer)
            (symbol-function 'ar:helm-create-helm-buffer)))
      (helm helm-c-source-rdefs))))

(defun helm-c-rdefs-init ()
  (let ((file-path (buffer-file-name)))
    (with-current-buffer (helm-candidate-buffer 'global)
      (ar:execute-rdefs file-path))))

(defun helm-c-rdefs-action (candidate)
  (ar:awhen (ar:substring-line-number candidate)
         (goto-line (string-to-number it))
         (recenter ar:recenter-height)))

(defun ar:clean-up ()
  (when (overlayp ar:current-line-overlay)
    (delete-overlay ar:current-line-overlay)))

(defun ar:persistent-action (candidate)
  (ar:awhen (ar:substring-line-number candidate)
     (goto-line (string-to-number it))
     (recenter ar:recenter-height)
     (when (overlayp ar:current-line-overlay)
       (move-overlay ar:current-line-overlay
                     (line-beginning-position)
                     (line-end-position)
                     (current-buffer))
       (overlay-put ar:current-line-overlay 'face 'highlight))))

(defun ar:helm-execute-persistent-action ()
  (when ar:enable-auto-look-flag
    (unless (zerop (buffer-size (get-buffer (helm-buffer-get))))
      (helm-execute-persistent-action))))

(defvar ar:helm-map
  (let ((map (copy-keymap helm-map)))
    (define-key map (kbd "C-n")  'ar:next-line)
    (define-key map (kbd "C-p")  'ar:previous-line)
    map))

(defun ar:next-line ()
  (interactive)
  (helm-next-line)
  (ar:helm-execute-persistent-action))

(defun ar:previous-line ()
  (interactive)
  (helm-previous-line)
  (ar:helm-execute-persistent-action))

(defun ar:display-buffer (buf)
  (delete-other-windows)
  (split-window (selected-window) nil t)
  (pop-to-buffer buf))

(defun ar:execute-rdefs (file-path)
  (interactive)
  (let ((command ar:command)
        (option "-n"))
    (call-process-shell-command (format "%s %s %s" command file-path option) nil t t)))

(defun ar:helm-create-helm-buffer (&optional test-mode)
  (with-current-buffer (get-buffer-create helm-buffer)
    (helm-log "kill local variables: %S" (buffer-local-variables))
    (kill-all-local-variables)
    (buffer-disable-undo)
    (erase-buffer)
    (ruby-mode)
    (when (fboundp 'linum-mode)
      (linum-mode -1))
    (set (make-local-variable 'inhibit-read-only) t)
    (set (make-local-variable 'helm-last-sources-local) helm-sources)
    (set (make-local-variable 'helm-follow-mode) nil)
    (set (make-local-variable 'helm-display-function) helm-display-function)
    (helm-log-eval helm-display-function helm-let-variables)
    (loop for (var . val) in helm-let-variables
          do (set (make-local-variable var) val))
    (setq cursor-type nil)
    (setq mode-name "Helm")
    )
  (helm-initialize-overlays helm-buffer)
  (get-buffer helm-buffer))


(provide 'helm-rdefs)
