(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(custom-set-variables
 '(el-get-verbose t))

;; setup
(el-get-bundle emacs-jp/init-loader)
(el-get-bundle purcell/exec-path-from-shell)



;; auto-complete
(el-get-bundle auto-complete)
(el-get-bundle fuzzy)
(el-get-bundle popup)

;; editing utilities
(el-get-bundle expand-region)
;(el-get-bundle wrap-region)
(el-get-bundle undo-tree)
(el-get-bundle multiple-cursors)
(el-get-bundle smartrep)
(el-get-bundle yasnippet)
(el-get-bundle goto-chg)
(el-get-bundle smartparens)
(el-get-bundle ag)
(el-get-bundle highlight-symbol)

;; buffer utils
(el-get-bundle elscreen)
(el-get-bundle yascroll)
(el-get-bundle helm-swoop)
(el-get-bundle revbufs)

;;flymake
(el-get-bundle flycheck)

(el-get-bundle coffee-mode)
(el-get-bundle sass-mode)
(el-get-bundle scss-mode)
(el-get-bundle less-css-mode)
(el-get-bundle haml-mode)
(el-get-bundle jade-mode)
(el-get-bundle go-mode)
(el-get-bundle dougm/goflymake)
(el-get-bundle go-eldoc)
(el-get-bundle php-mode)
(el-get-bundle rvm)
(el-get-bundle ruby-block)
(el-get-bundle ruby-tools)
(el-get-bundle dockerfile-mode)
(el-get-bundle swift-mode)
(el-get-bundle code-mancers/rspec-simple)
(el-get-bundle csharp-mode)
(el-get-bundle jedi)
(el-get-bundle py-autopep8)
(el-get-bundle py-isort)
;(el-get-bundle omnisharp)

;;;; markup language
(el-get-bundle htmlize)
(el-get-bundle web-mode)
(el-get-bundle yaml-mode)
(el-get-bundle emmet-mode)
(el-get-bundle markdown-mode)
;(el-get-bundle markdown-mode+)
(el-get-bundle textile-mode)

;; emacs-lisp
(el-get-bundle elisp-slime-nav)
(el-get-bundle thingopt)

;; common utility
(el-get-bundle quickrun)

;; helm
(el-get-bundle helm)
(el-get-bundle helm-descbinds)
(el-get-bundle helm-projectile)

;; VCS
(el-get-bundle magit)
(el-get-bundle git-gutter)
;(el-get-bundle git-commit-mode)
;(el-get-bundle git-rebase-mode)

;; directory operation
(el-get-bundle direx)
(el-get-bundle pkg-info)
(el-get-bundle projectile)

;; basic
(el-get-bundle init-loader)
;(el-get-bundle solarized-theme)
;(el-get-bundle zenburn-theme)
(el-get-bundle moe-theme)
(el-get-bundle exec-path-from-shell)
(el-get-bundle anzu)
(el-get-bundle emacswiki:redo+)
(el-get-bundle dash-at-point)
(el-get-bundle powerline)

;
(el-get-bundle japanese-holidays)

;;; TypeScript
(el-get-bundle typescript-mode)
(el-get-bundle tide)
;; (use-package tide
;;   :config
;;   (defun typescript-mode-hooks-for-tide ()
;;     (tide-setup)
;;     (flycheck-mode t)
;;     (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;     (eldoc-mode t)
;;     (company-mode-on))
;;   (add-hook 'typescript-mode-hook 'typescript-mode-hooks-for-tide))
