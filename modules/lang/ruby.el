;;; ruby.el ---  ruby packages -*- lexical-binding: t; -*-

(spacemacs|define-jump-handlers enh-ruby-mode)
(spacemacs|define-jump-handlers ruby-mode)

(defun spacemacs//inf-ruby-auto-enter ()
  "Automatically enters inf-ruby-mode in ruby modes' debugger breakpoints."
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter nil t))

(defun spacemacs//ruby-enable-minitest-mode ()
  "Conditionally enable `minitest-mode'"
  (when (eq 'minitest ruby-test-runner)
    (minitest-enable-appropriate-mode)))

(defvar ruby-enable-enh-ruby-mode t
  "If non-nil, use `enh-ruby-mode' package instead of the built-in Ruby Mode.")

(defvar ruby-test-runner 'minitest
  "Test runner to use. Possible values are `ruby-test', `minitest' or `rspec'.")

(use-package bundler
  :straight t
  :defer t)

(use-package robe
  :straight t
  :defer t
  :diminish robe-mode
  :init
  (spacemacs/register-repl 'robe 'robe-start "robe")
  (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
    (add-hook hook 'robe-mode))
  (spacemacs/add-to-hooks 'robe-jump
                          '(spacemacs-jump-handlers-ruby-mode
                            spacemacs-jump-handlers-enh-ruby-mode)))

(use-package rspec-mode
  :straight t
  :defer t
  :diminish rspec-mode
  :init
  (spacemacs/add-to-hooks 'spacemacs//ruby-enable-rspec-mode
                          '(ruby-mode-local-vars-hook
                            enh-ruby-mode-local-vars-hook))
  ;; remove hooks automatically added by rspec via autoload
  ;; because we want to be able to control when rspec-mode is
  ;; loaded based on the layer variable `ruby-test-runner'
  (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
    (remove-hook hook 'rspec-enable-appropriate-mode))
  :config
  (add-hook 'rspec-compilation-mode-hook 'spacemacs//inf-ruby-auto-enter))

(use-package minitest
  :straight t
  :defer t
  :diminish minitest-mode
  :init
  (spacemacs/add-to-hooks 'spacemacs//ruby-enable-minitest-mode
                          '(ruby-mode-local-vars-hook
                            enh-ruby-mode-local-vars-hook))
  ;; remove hooks added by minitest mode
  (dolist (hook '(ruby-mode-hook enh-ruby-mode-hook))
    (remove-hook hook 'minitest-enable-appropriate-mode)))

(use-package rake
  :straight t
  :defer t
  :init (setq rake-cache-file (concat spacemacs-cache-directory "rake.cache")))

(use-package ruby-mode
  :defer t
  :mode (("Appraisals\\'" . ruby-mode)
         ("Puppetfile" . ruby-mode))
  :init
  (defun spacemacs/ruby-maybe-highlight-debugger-keywords ()
    "Highlight break point lines."
    (interactive)
    (when ruby-highlight-debugger-keywords
      (highlight-lines-matching-regexp "byebug")
      (highlight-lines-matching-regexp "binding.irb")
      (highlight-lines-matching-regexp "binding.pry")))
  (spacemacs/add-to-hooks
   'spacemacs/ruby-maybe-highlight-debugger-keywords
   '(ruby-mode-local-vars-hook enh-ruby-mode-local-vars-hook))
  :config
  (define-key ruby-mode-map (kbd "C-c C-z") #'rtog/toggle-repl)
  (evil-define-key 'normal 'ruby-mode
    ",'" 'ruby-toggle-string-quotes
    ",{" 'ruby-toggle-block)
  (sp-with-modes (if ruby-enable-enh-ruby-mode 'enh-ruby-mode 'ruby-mode)
    (sp-local-pair
     "{" "}"
     :pre-handlers '(sp-ruby-pre-handler)
     :post-handlers '(sp-ruby-post-handler
                      (spacemacs/smartparens-pair-newline-and-indent "RET"))
     :suffix ""))
  (push '("*rspec-compilation*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
        popwin:special-display-config)
  (push '("*rake-compilation*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
        popwin:special-display-config))


(use-package enh-ruby-mode
  :straight t
  :mode (("Appraisals\\'" . enh-ruby-mode)
         ("\\(Rake\\|Thor\\|Guard\\|Gem\\|Cap\\|Vagrant\\|Berks\\|Pod\\|Puppet\\)file\\'" . enh-ruby-mode)
         ("\\.\\(rb\\|rabl\\|ru\\|builder\\|rake\\|thor\\|gemspec\\|jbuilder\\)\\'" . enh-ruby-mode))
  :interpreter "ruby"
  :diminish enh-ruby-mode
  :init
  (setq enh-ruby-deep-indent-paren nil
        enh-ruby-hanging-paren-deep-indent-level 2)
  :config
  (define-key enh-ruby-mode-map (kbd "C-c C-z") #'rtog/toggle-repl)
  (dolist (map (list enh-ruby-mode-map))
    (evil-define-key* 'normal map
                      ",kk"    'rake
                      ",kr"    'rake-rerun
                      ",kR"    'rake-regenerate-cache
                      ",kf"    'rake-find-task))
  (dolist (map (list  enh-ruby-mode-map))
    (evil-define-key* 'normal map
                      ",ta" 'minitest-verify-all
                      ",tb" 'minitest-verify
                      ",tr" 'minitest-rerun
                      ",ts" 'minitest-verify-single))
  (dolist (map (list  enh-ruby-mode-map))
    (evil-define-key* 'normal map
                      ",ta"    'rspec-verify-all
                      ",tb"    'rspec-verify
                      ",tc"    'rspec-verify-continue
                      ",td"    'ruby/rspec-verify-directory
                      ",te"    'rspec-toggle-example-pendingness
                      ",tf"    'rspec-verify-method
                      ",tl"    'rspec-run-last-failed
                      ",tm"    'rspec-verify-matching
                      ",tr"    'rspec-rerun
                      ",tt"    'rspec-verify-single
                      ",t~"    'rspec-toggle-spec-and-target-find-example
                      ",t TAB" 'rspec-toggle-spec-and-target))
  (dolist (map (list  enh-ruby-mode-map))
    (evil-define-key* 'normal map
                      ",bc" 'bundle-check
                      ",bi" 'bundle-install
                      ",bs" 'bundle-console
                      ",bu" 'bundle-update
                      ",bx" 'bundle-exec
                      ",bo" 'bundle-open))
  (dolist (map (list enh-ruby-mode-map))
    (evil-define-key* 'normal map
                      ",'" 'robe-start
                      ;; robe mode specific
                      ",hh" 'robe-doc
                      ",rsr" 'robe-rails-refresh
                      ;; inf-enh-ruby-mode
                      ",sf" 'ruby-send-definition
                      ",sF" 'ruby-send-definition-and-go
                      ",si" 'robe-start
                      ",sr" 'ruby-send-region
                      ",sR" 'ruby-send-region-and-go
                      ",ss" 'ruby-switch-to-inf)))

(use-package inf-ruby
  :after (ruby-mode enh-ruby-mode)
  :defer t
  :config
  (define-key inf-ruby-mode-map (kbd "C-c C-z") #'rtog/toggle-repl)
  (define-key inf-ruby-minor-mode-map (kbd "C-c C-z") #'rtog/toggle-repl)
  (add-hook 'compilation-filter-hook #'inf-ruby-auto-enter t)
  (add-hook 'after-init-hook #'inf-ruby-switch-setup))