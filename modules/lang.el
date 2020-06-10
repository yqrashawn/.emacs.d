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
;; (yq/get-modules "lang/rust.el")
;; (yq/get-modules "lang/c.el")
(yq/get-modules "lang/vue.el")
;; (yq/get-modules "lang/python.el")
(yq/get-modules "lang/md.el")
(yq/get-modules "lang/ruby.el")
(yq/get-modules "lang/html.el")
(yq/get-modules "lang/css.el")
(yq/get-modules "lang/lua.el")
(yq/get-modules "lang/clojure.el")
(yq/get-modules "lang/restclient.el")
(yq/get-modules "lang/sql.el")
(yq/get-modules "lang/plantuml.el")
;; (yq/get-modules "lang/solidity.el")

(use-package adoc-mode
  :straight t
  :defer t
  :mode ("\\\.adoc\\\'" . adoc-mode))

(use-package yaml-mode
  :straight t
  :defer t
  :mode ("\\\.yaml\\\'" . yaml-mode))

(use-package groovy-mode
  :straight t
  :mode (("\\\Jenkinsfile\\\'" . groovy-mode))
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
  :mode (("\\\.env\\..*\\\'" . dotenv-mode)))

(use-feature conf-mode
  :straight t
  :mode (("\\\.conf\\\'"    . conf-space-mode)
         ("\\\.setup.*\\\'" . conf-space-mode)
         ("/\\(Cargo.lock\\|\\.cargo/config\\)\\\'" . conf-toml-mode))
  :init
  (add-to-list 'auto-mode-alist '("\\.[^b][^a][a-zA-Z]*rc$" . conf-mode))
  (add-to-list 'auto-mode-alist '("\\.aspell\\.en\\.pws\\'" . conf-mode))
  (add-to-list 'auto-mode-alist '("\\mimeapps\\.list$" . conf-mode))
  (add-to-list 'auto-mode-alist '("\\.editorconfig$" . conf-mode))
  (add-to-list 'auto-mode-alist '("\\.meta\\'" . conf-mode))
  (add-to-list 'auto-mode-alist '("\\.?muttrc\\'" . conf-mode))
  (add-to-list 'auto-mode-alist '("\\.mailcap\\'" . conf-mode)))

(use-package crontab-mode
  :straight (:host github :repo "emacs-pe/crontab-mode")
  :defer t)

(defun lsp-eslint-fix-before-save ()
  (add-hook 'before-save-hook #'lsp-eslint-apply-all-fixes))

;; http://blog.binchen.org/posts/how-to-speed-up-lsp-mode.html
;; no real time syntax check
(use-package lsp-mode
  :straight t
  :hook ((dockerfile-mode shell-script-mode web-mode css-mode typescript-mode js2-mode rjsx-mode) . lsp-deferred)
  :hook ((js2-mode js-mode rjsx-mode) . lsp-eslint-fix-before-save)
  :custom
  ;; lsp-mode
  (lsp-keep-workspace-alive nil)
  (lsp-enable-semantic-highlighting t)
  (lsp-restart 'auto-restart)
  (lsp-eldoc-enable-hover t)
  (lsp-eldoc-render-all nil)
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-on-type-formatting t)
  (lsp-imenu-sort-methods '(position))
  (lsp-prefer-capf t)
  (lsp-symbol-highlighting-skip-current nil)
  (lsp-idle-delay 0.500)
  ;; lsp-clients
  (lsp-bash-explainshell-endpoint t)
  (lsp-bash-highlight-parsing-errors t)
  (lsp-bash-glob-pattern t)
  ;; ts-js
  (lsp-eslint-server-command
   `("node" ,(expand-file-name (car (last
                                     (file-expand-wildcards
                                      "~/.vscode/extensions/dbaeumer.vscode-eslint-*/server/out/eslintServer.js")))) "--stdio"))
  (lsp-eslint-package-manager "yarn")
  (lsp-eslint-auto-fix-on-save t)
  (lsp-eslint-run "onSave")
  (lsp-eslint-package-manager "yarn")
  (lsp-clojure-server-command '("bash" "-c" "clojure-lsp"))
  ;; (lsp-enable-indentation nil)
  :config
  (add-hook
   'lsp-managed-mode-hook
   (defl (setq-local company-minimum-prefix-length 1)
     (setq-local company-idle-delay 0.0)))
  ;; temp fix company-lsp
  (defun yq/lsp-adjust-company-backends ()
    (setq-local company-backends (cons 'company-tabnine (cons 'company-lsp (remove 'company-capf (remove 'company-lsp (remove 'company-tabnine company-backends)))))))
  (add-hook 'lsp-after-open-hook 'yq/lsp-adjust-company-backends)
  (defun spacemacs//setup-lsp-jump-handler (&rest modes)
    "Set jump handler for LSP with the given MODE."
    (dolist (m modes)
      (add-to-list (intern (format "spacemacs-jump-handlers-%S" m))
                   '(lsp-find-definition :async t))))
  (add-hook
   'lsp-after-open-hook
   (defl (spacemacs//setup-lsp-jump-handler major-mode)))
  (push '("*lsp-help*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
        popwin:special-display-config)
  ;; clojure-lsp https://github.com/snoe/clojure-lsp#installation
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure"))))

(use-package lsp-ui
  :straight t
  :disabled
  :commands lsp-ui-mode
  :custom
  ;; top right stuff
  ;; disable for now
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-delay 0.5)
  ;; header is useless
  (lsp-ui-doc-header t)
  ;; same as eldoc
  (lsp-ui-doc-include-signature t)
  (lsp-ui-flycheck-enable nil)

  ;; sideline
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-delay 0.5)
  ;; idicating which symbol cursor is on
  (lsp-ui-sideline-show-symbol t)
  ;; wether show hoverd line js info (type info?)
  (lsp-ui-sideline-show-hover nil)
  ;; the up text show what the codes doing
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-peek-always-show nil))

(use-package company-lsp
  :straight t
  :disabled t
  :after company
  :commands (company-lsp)
  :custom
  (company-lsp-async t)
  (company-lsp-cache-candidates (if *imac* nil 'auto))
  (company-lsp-enable-recompletion (if *imac* t nil)))

(use-package lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)

(use-package image+
  :straight t
  :after 'image-mode
  :hook (image-mode . image+)
  :bind ((:map image-mode-map
               ("0" . imagex-sticky-restore-original)
               ("+" . imagex-sticky-maximize)
               ("=" . imagex-sticky-zoom-in)
               ("-" . imagex-sticky-zoom-out))))

(use-package lsp-ivy
  :straight t
  :defer t)

(use-package dockerfile-mode
  :straight t
  :mode ("Dockerfile\\'" . dockerfile-mode))