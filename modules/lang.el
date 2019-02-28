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
