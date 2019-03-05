(use-feature sh-script
  :mode
  ("\\.*zshrc" . sh-mode)
  ("\\.*zshenv" . sh-mode)
  ("\\.*zprofile" . sh-mode)
  ("\\.*bashrc" . sh-mode)
  ("\\.*bash_profile" . sh-mode)
  :config
  (setq-default sh-indentation 2
                sh-basic-offset 2))

(yq/get-modules "lang/elisp.el")
(yq/get-modules "lang/js2.el")
(yq/get-modules "lang/typescript.el")
(yq/get-modules "lang/org.el")
;; (yq/get-modules "lang/rust.el")
;; (yq/get-modules "lang/c.el")
(yq/get-modules "lang/vue.el")
;; (yq/get-modules "lang/python.el")
(yq/get-modules "lang/md.el")
(yq/get-modules "lang/ruby.el")
(yq/get-modules "lang/html.el")
(yq/get-modules "lang/css.el")
;; (yq/get-modules "lang/lua.el")
(yq/get-modules "lang/clojure.el")
(yq/get-modules "lang/restclient.el")

(use-package yaml-mode
  :straight t
  :defer t
  :mode ("\\.yaml\\'" . yaml-mode))

(use-package groovy-mode
  :straight t
  :mode (("\\Jenkinsfile\\'" . groovy-mode))
  :init
  (setq groovy-indent-offset 2))

(use-package nginx-mode
  :straight t
  :mode (("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)
         ("nginx.conf" . nginx-mode)))

(use-package company-nginx
  :straight t
  :after nginx-mode
  :hook (nginx-mode . company-nginx-keywords))

(use-package dotenv-mode
  :straight t
  :mode (("\\.env\\..*\\'" . dotenv-mode)))

(use-package conf-mode
  :straight t
  :mode (("\\.conf\\'"    . conf-space-mode)
         ("\\.setup.*\\'" . conf-space-mode)
         ("/\\(Cargo.lock\\|\\.cargo/config\\)\\'" . conf-toml-mode)))

(use-package crontab-mode
  :straight (:host github :repo "emacs-pe/crontab-mode")
  :defer t)

(use-package lsp-mode
  :straight t
  :hook ((shell-script-mode web-mode css-mode typescript-mode js2-mode) . lsp-mode)
  :custom
  (lsp-auto-guess-root nil)
  (lsp-restart 'auto-restart)
  (lsp-prefer-flymake nil)
  :config
  (defun spacemacs//setup-lsp-jump-handler (&rest modes)
    "Set jump handler for LSP with the given MODE."
    (dolist (m modes)
      (add-to-list (intern (format "spacemacs-jump-handlers-%S" m))
                   '(lsp-ui-peek-find-definitions :async t))))
  (add-hook
   'lsp-after-open-hook
   (defl (spacemacs//setup-lsp-jump-handler major-mode))))

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature nil)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-update-mode 'line)
  (lsp-ui-sideline-delay 0.5)
  (lsp-ui-peek-always-show nil)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (define-key lsp-ui-peek-mode-map "h" #'lsp-ui-peek--select-prev-file)
  (define-key lsp-ui-peek-mode-map "j" #'lsp-ui-peek--select-next)
  (define-key lsp-ui-peek-mode-map "k" #'lsp-ui-peek--select-prev)
  (define-key lsp-ui-peek-mode-map "l" #'lsp-ui-peek--select-next-file))

(use-package company-lsp
  :straight t
  :after company
  :custom
  (company-lsp-cache-candidates 'auto)
  :config
  (push 'company-lsp company-backends))

