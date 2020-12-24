;;; packages for variables config files and lsp
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

;;;; load modes for languages
(yq/get-modules "lang/elisp.el")
(yq/get-modules "lang/js2.el")
(yq/get-modules "lang/typescript.el")
(yq/get-modules "lang/rust.el")
;; (yq/get-modules "lang/c.el")
;; (yq/get-modules "lang/vue.el")
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

;; http://blog.binchen.org/posts/how-to-speed-up-lsp-mode.html
;; no real time syntax check
(use-package lsp-mode
  :straight t
  :init
  (defun lsp-eslint-fix-before-save ()
    (add-hook 'before-save-hook #'lsp-eslint-fix-all nil 'make-it-local))
  (defun +lsp-organize-imports ()
    (add-hook 'before-save-hook #'lsp-organize-imports nil 'make-it-local))
  :hook (((json-mode dockerfile-mode shell-script-mode web-mode css-mode typescript-mode js2-mode rjsx-mode ;; clojure-mode clojurescript-mode
                     ) . lsp-deferred)
         ;; ((typescript-mode js2-mode js-mode rjsx-mode) . +lsp-organize-imports)
         ;; ((typescript-mode js2-mode js-mode rjsx-mode) . lsp-eslint-fix-before-save)
         )
  :custom
  ;; lsp-mode
  (lsp-before-save-edits nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-eslint-enable nil)
  (lsp-file-watch-threshold 4000)
  (lsp-keep-workspace-alive nil)
  (lsp-enable-semantic-highlighting t)
  (lsp-restart 'auto-restart)
  (lsp-imenu-sort-methods '(position))
  (lsp-symbol-highlighting-skip-current nil)
  (lsp-idle-delay 0.5)
  ;; lsp-clients
  (lsp-bash-highlight-parsing-errors t)
  ;; ts-js
  (lsp-eslint-package-manager "yarn")
  (lsp-eslint-auto-fix-on-save t)
  (lsp-eslint-run "onSave")
  (lsp-enable-on-type-formatting nil)
  (lsp-disabled-clients '(javascript-typescript-langserver))
  :config
  (add-to-list #'lsp-file-watch-ignored "[/\\\\]conflux-portal[/\\\]builds$")
  (add-to-list #'lsp-file-watch-ignored "[/\\\\]conflux-portal[/\\\]dist$")
  (add-to-list #'lsp-file-watch-ignored "[/\\\\]coverage$")
  (add-to-list #'lsp-file-watch-ignored "[/\\\\]node_modules$")
  (defun +lsp-workspace-folders-add (workspace)
    (interactive
     (list (read-directory-name "Select folder to add: "
                                (or (lsp--suggest-project-root) default-directory) nil t)))
    (let* ((dir (expand-file-name workspace))
           (files (nthcdr 2 (directory-files dir)))
           (projects (seq-filter (lambda (file) (file-directory-p (concat dir file))) files)))
      (mapcar (lambda (project) (lsp-workspace-folders-add (concat dir project))) projects)))
  (add-hook
   'lsp-managed-mode-hook
   (defl (setq-local company-minimum-prefix-length 1)
     (setq-local company-idle-delay 0.0)))
  ;; temp fix company-lsp
  (defun yq/lsp-adjust-company-backends ()
    (setq-local company-backends (cons 'company-tabnine (cons 'company-capf (remove 'company-capf (remove 'company-tabnine company-backends)))))
    ;; (setq-local company-backends (cons 'company-lsp (remove 'company-capf (remove 'company-lsp company-backends))))
    )
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
  ;; lsp ui doc
  ;; disable for now
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-delay 2)
  ;; header is useless
  (lsp-ui-doc-header t)
  ;; same as eldoc
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-include-signature t)

  ;; sideline
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-delay 2)
  ;; idicating which symbol cursor is on
  (lsp-ui-sideline-show-symbol t)
  ;; wether show hoverd line js info (type info?)
  (lsp-ui-sideline-show-hover t)
  ;; the up text show what the codes doing
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-peek-always-show nil))

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

(use-package langtool
  :straight t
  :commands (langtool-check)
  :custom
  (langtool-bin "languagetool"))