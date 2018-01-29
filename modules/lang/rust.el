(spacemacs|define-jump-handlers rust-mode)
(let ((var "RUST_SRC_PATH"))
  (unless (or (member var exec-path-from-shell-variables) (getenv var))
    (push var exec-path-from-shell-variables)))

(use-package rust-mode
  :straight t
  :defer t
  :init
  (spacemacs|add-company-backends
    :backends company-capf
    :modes rust-mode
    :variables company-tooltip-align-annotations t)
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
  (evil-define-key 'normal rust-mode-map
    ",=" 'rust-format-buffer
    ",q" 'spacemacs/rust-quick-run))

(use-package cargo
  :straight t
  :defer t
  :init
  (evil-define-key 'normal rust-mode-map
    ",c." 'cargo-process-repeat
    ",cC" 'cargo-process-clean
    ",cX" 'cargo-process-run-example
    ",cc" 'cargo-process-build
    ",cd" 'cargo-process-doc
    ",cD" 'cargo-process-doc-open
    ",ce" 'cargo-process-bench
    ",cf" 'cargo-process-current-test
    ",cf" 'cargo-process-fmt
    ",ci" 'cargo-process-init
    ",cl" 'cargo-process-clippy
    ",cn" 'cargo-process-new
    ",co" 'cargo-process-current-file-tests
    ",cs" 'cargo-process-search
    ",cu" 'cargo-process-update
    ",cx" 'cargo-process-run
    ",t" 'cargo-process-test))

(use-package flycheck-rust
  :straight t
  :defer t
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (spacemacs/enable-flycheck 'rust-mode))

(use-package toml-mode
  :straight t
  :mode "/\\(Cargo.lock\\|\\.cargo/config\\)\\'")
(with-eval-after-load 'smartparens
  ;; Don't pair lifetime specifiers
  (sp-local-pair 'rust-mode "'" nil :actions nil))

(use-package racer
  :straight t
  :defer t
  :diminish racer-mode
  :init
  (defun spacemacs/racer-describe ()
    "Show a *Racer Help* buffer for the function or type at point.
If `help-window-select' is non-nil, also select the help window."
    (interactive)
    (let ((window (racer-describe)))
      (when help-window-select
        (select-window window))))
  (spacemacs/add-to-hook 'rust-mode-hook '(racer-mode))
  (add-to-list 'spacemacs-jump-handlers-rust-mode 'racer-find-definition)
  (evil-define-key 'normal rust-mode-map
    "hh" 'spacemacs/racer-describe)
  :config
  (evilified-state-evilify-map racer-help-mode-map
    :mode racer-help-mode))
