;;; rust.el ---  rust packages -*- lexical-binding: t; -*-

(spacemacs|define-jump-handlers rustic-mode)
(let ((var "RUST_SRC_PATH"))
  (unless (or (member var exec-path-from-shell-variables) (getenv var))
    (push var exec-path-from-shell-variables)))

(use-package rustic
  :straight t
  :mode (("\\.rs$" . rustic-mode))
  :commands (rustic-run-cargo-command rustic-cargo-outdated)
  :custom
  (rustic-indent-method-chain t)
  (rustic-format-trigger 'on-save)
  :init
  (with-eval-after-load 'org-src
    (defalias 'org-babel-execute:rust #'org-babel-execute:rustic)
    (add-to-list 'org-src-lang-modes '("rust" . rustic)))

  (defun spacemacs/rust-quick-run ()
    "Quickly run a Rust file using rustc.
Meant for a quick-prototype flow only - use `spacemacs/open-junk-file' to
open a junk Rust file, type in some code and quickly run it.
If you want to use third-party crates, create a a new project using `cargo-process-new' and run
using `cargo-process-run'."
    (interactive)
    (let ((input-file-name (buffer-file-name))
          (output-file-name (concat temporary-file-directory (make-temp-name "rustbin"))))
      (compile
       (format "rustc -o %s %s && %s"
               (shell-quote-argument output-file-name)
               (shell-quote-argument input-file-name)
               (shell-quote-argument output-file-name)))))
  :config
  (evil-define-key 'normal rustic-mode-map
    ",=" 'rust-format-buffer
    ",q" 'spacemacs/rust-quick-run))

(use-package cargo
  :defer t
  :init
  (push '("*Cargo Run*" :dedicated t :position bottom :stick t :noselect t :height 0.4) popwin:special-display-config)
  (push '("*Cargo Test*" :dedicated t :position bottom :stick t :noselect t :height 0.4) popwin:special-display-config)
  (push '("*Cargo Clippy*" :dedicated t :position bottom :stick t :noselect t :height 0.4) popwin:special-display-config)
  (evil-define-key 'normal 'rustic-mode
    ",c." 'cargo-process-repeat
    ",ca" 'cargo-process-add
    ",cA" 'cargo-process-audit
    ",cc" 'cargo-process-build
    ",cC" 'cargo-process-clean
    ",cd" 'cargo-process-doc
    ",cD" 'cargo-process-doc-open
    ",ce" 'cargo-process-bench
    ",cE" 'cargo-process-run-example
    ",cf" 'cargo-process-fmt
    ",ci" 'cargo-process-init
    ",cl" 'cargo-process-clippy
    ",cn" 'cargo-process-new
    ",co" 'cargo-process-current-file-tests
    ",cr" 'cargo-process-rm
    ",cs" 'cargo-process-search
    ",ct" 'cargo-process-current-test
    ",cu" 'cargo-process-update
    ",cU" 'cargo-process-upgrade
    ",cx" 'cargo-process-run
    ",cX" 'cargo-process-run-bin
    ",cv" 'cargo-process-check
    ",t" 'cargo-process-test))

;; (use-package flycheck-rust
;;   :straight t
;;   :defer t
;;   :init
;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
;;   (spacemacs/enable-flycheck 'rustic-mode))

(use-package toml-mode
  :straight t
  :mode "/\\(Cargo.lock\\|\\.cargo/config\\)\\'")

(with-eval-after-load 'smartparens
  ;; Don't pair lifetime specifiers
  (sp-local-pair 'rustic-mode "'" nil :actions nil))

(use-package racer
  :defer t
  :diminish racer-mode
  :commands racer-mode
  :config
  (spacemacs/add-to-hook 'rustic-mode-hook '(racer-mode))
  (spacemacs/add-to-hook 'racer-mode-hook '(eldoc-mode))
  (add-to-list 'spacemacs-jump-handlers-rust-mode 'racer-find-definition)
  (evil-define-key 'normal 'rustic-mode
    ",hh" 'spacemacs/racer-describe)
  (evilified-state-evilify-map racer-help-mode-map :mode racer-help-mode))