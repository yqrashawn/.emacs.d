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
(yq/get-modules "lang/clojure.el")
(yq/get-modules "lang/lua.el")
(yq/get-modules "lang/restclient.el")
(yq/get-modules "lang/sql.el")
(yq/get-modules "lang/plantuml.el")
(yq/get-modules "lang/solidity.el")

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
  :commands (lsp-install-server)
  :custom
  (lsp-session-file (concat spacemacs-cache-directory "lsp-session"))
  (lsp-intelephense-storage-path (concat spacemacs-cache-directory "lsp-intelephense/"))
  ;; lsp-mode
  (lsp-enable-folding nil)
  (lsp-enable-text-document-color nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-diagnostics-provider :flymake)
  (lsp-before-save-edits nil)
  (lsp-file-watch-threshold 4000)
  (lsp-keep-workspace-alive nil)
  (lsp-enable-semantic-highlighting nil)
  (lsp-restart 'auto-restart)
  (lsp-imenu-sort-methods '(position))
  (lsp-symbol-highlighting-skip-current nil)
  (lsp-idle-delay 0.5)
  ;; lsp-clients
  (lsp-bash-highlight-parsing-errors t)
  ;; ts-js
  (lsp-eslint-enable nil)
  (lsp-eslint-package-manager "yarn")
  (lsp-eslint-auto-fix-on-save t)
  (lsp-eslint-run "onSave")
  (lsp-disabled-clients '(javascript-typescript-langserver))
  (lsp-clients-typescript-plugins (vector
                                   (list
                                    :name "@vsintellicode/typescript-intellicode-plugin"
                                    :location "~/.vscode/extensions/visualstudioexptteam. vscodeintellicode-1.2.11/")))
  (lsp-eldoc-enable-hover nil)
  (lsp-signature-auto-activate t)
  (lsp-signature-doc-lines 1)
  :hook (((json-mode dockerfile-mode shell-script-mode web-mode css-mode typescript-mode js2-mode rjsx-mode ;; clojure-mode clojurec-mode clojurescript-mode
                     ) . lsp-deferred)
         ;; ((typescript-mode js2-mode js-mode rjsx-mode) . +lsp-organize-imports)
         ;; ((typescript-mode js2-mode js-mode rjsx-mode) . lsp-eslint-fix-before-save)
         )
  :init
  (defun lsp-eslint-fix-before-save ()
    (add-hook 'before-save-hook #'lsp-eslint-fix-all nil 'make-it-local))
  (defun +lsp-organize-imports ()
    (add-hook 'before-save-hook #'lsp-organize-imports nil 'make-it-local))
  (defvar +lsp--default-read-process-output-max nil)
  (defvar +lsp--default-gcmh-high-cons-threshold nil)
  (defvar +lsp--optimization-init-p nil)

  (define-minor-mode +lsp-optimization-mode
    "Deploys universal GC and IPC optimizations for `lsp-mode' and `eglot'."
    :global t
    :init-value nil
    (if (not +lsp-optimization-mode)
        (setq-default read-process-output-max +lsp--default-read-process-output-max
                      gcmh-high-cons-threshold +lsp--default-gcmh-high-cons-threshold
                      +lsp--optimization-init-p nil)
      ;; Only apply these settings once!
      (unless +lsp--optimization-init-p
        (setq +lsp--default-read-process-output-max
              ;; DEPRECATED Remove check when 26 support is dropped
              (if (boundp 'read-process-output-max)
                  (default-value 'read-process-output-max))
              +lsp--default-gcmh-high-cons-threshold
              (default-value 'gcmh-high-cons-threshold))
        ;; `read-process-output-max' is only available on recent development
        ;; builds of Emacs 27 and above.
        (setq-default read-process-output-max (* 1024 1024))
        ;; REVIEW LSP causes a lot of allocations, with or without Emacs 27+'s
        ;;        native JSON library, so we up the GC threshold to stave off
        ;;        GC-induced slowdowns/freezes. Doom uses `gcmh' to enforce its
        ;;        GC strategy, so we modify its variables rather than
        ;;        `gc-cons-threshold' directly.
        (setq-default gcmh-high-cons-threshold (* 2 +lsp--default-gcmh-high-cons-threshold))
        (gcmh-set-high-threshold)
        (setq +lsp--optimization-init-p t))))
  :config
  (defadvice! +lsp--respect-user-defined-checkers-a (orig-fn &rest args)
    "Ensure user-defined `flycheck-checker' isn't overwritten by `lsp'."
    :around #'lsp-diagnostics-flycheck-enable
    (if flycheck-checker
        (let ((old-checker flycheck-checker))
          (apply orig-fn args)
          (setq-local flycheck-checker old-checker))
      (apply orig-fn args)))

  (add-hook! 'lsp-mode-hook
    (defun +lsp-display-guessed-project-root-h ()
      "Log what LSP things is the root of the current project."
      ;; Makes it easier to detect root resolution issues.
      (when-let (path (buffer-file-name (buffer-base-buffer)))
        (if-let (root (lsp--calculate-root (lsp-session) path))
            (lsp--info "Guessed project root is %s" (abbreviate-file-name root))
          (lsp--info "Could not guess project root."))))
    #'+lsp-optimization-mode)

  (defvar +lsp-defer-shutdown 3
    "If non-nil, defer shutdown of LSP servers for this many seconds after last
workspace buffer is closed.

This delay prevents premature server shutdown when a user still intends on
working on that project after closing the last buffer, or when programmatically
killing and opening many LSP/eglot-powered buffers.")

  (defvar +lsp--deferred-shutdown-timer nil)
  (defadvice! +lsp-defer-server-shutdown-a (orig-fn &optional restart)
    "Defer server shutdown for a few seconds.
This gives the user a chance to open other project files before the server is
auto-killed (which is a potentially expensive process). It also prevents the
server getting expensively restarted when reverting buffers."
    :around #'lsp--shutdown-workspace
    (if (or lsp-keep-workspace-alive
            restart
            (null +lsp-defer-shutdown)
            (= +lsp-defer-shutdown 0))
        (prog1 (funcall orig-fn restart)
          (+lsp-optimization-mode -1))
      (when (timerp +lsp--deferred-shutdown-timer)
        (cancel-timer +lsp--deferred-shutdown-timer))
      (setq +lsp--deferred-shutdown-timer
            (run-at-time
             (if (numberp +lsp-defer-shutdown) +lsp-defer-shutdown 3)
             nil (lambda (workspace)
                   (with-lsp-workspace workspace
                     (unless (lsp--workspace-buffers workspace)
                       (let ((lsp-restart 'ignore))
                         (funcall orig-fn))
                       (+lsp-optimization-mode -1))))
             lsp--cur-workspace))))

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
    (when (boundp 'company-fuzzy-mode)
      (company-fuzzy-mode 0))
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

(use-package bazel-mode
  :straight t
  :disabled
  :commands (bazelrc-mode bazel-mode bazel-build-mode))
