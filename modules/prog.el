(use-package company
  :straight t
  :diminish company-mode
  :init
  (setq company-idle-delay 0
	company-minimum-prefix-length 2
	company-require-match nil
	company-dabbrev-ignore-case nil
	company-dabbrev-downcase nil)
  :config
  (setq company-backends '(company-capf
			   (company-dabbrev-code
			    company-gtags
			    company-etags
			    company-keywords)
			   company-files
			   company-dabbrev))
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous)
  (define-key company-active-map (kbd "C-l") 'company-complete-selection)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package flycheck
  :straight t)

(use-package smartparens
  :straight t
  :diminish smartparens-mode
  :config
  ;; (add-hook 'prog-mode #'smartparens-mode)
  (smartparens-global-mode t)
  (define-key evil-normal-state-map "sd" 'sp-kill-sexp)
  (use-package smartparens-config))

(use-package yasnippets
  :straight yasnippet
  :diminish yas-global-mode
  :commands (yas-global-mode yas-minor-mode)
  :hook (prog-mode-hook . yas-minor-mode)
  :init
  (setq yas-triggers-in-field t
	yas-wrap-around-region t)
  (setq yas-prompt-functions '(yas-completing-prompt))
  (setq yas-minor-mode-map (make-sparse-keymap))
  (define-key yas-minor-mode-map (kbd "M-s-/") 'yas-next-field)
  :config
  (push 'yas-hippie-try-expand hippie-expand-try-functions-list)
  (push 'yas-installed-snippets-dir yas-snippet-dirs))

;; TODO: auto-yas
