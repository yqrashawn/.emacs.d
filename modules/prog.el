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
  (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
  (define-key company-active-map (kbd "C-d") 'company-show-location)
  (define-key company-active-map (kbd "C-m") 'newline-and-indent)
  (define-key company-active-map (kbd "C-r") 'company-show-doc-buffer)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-flx
  :straight t
  :init
  (add-hook 'emacs-lisp-mode-hook 'company-flx-mode)
  :config
  (company-flx-mode +1))

(setq syntax-checking-enable-by-default t)

(defun spacemacs/enable-flycheck (mode)
  "Use flycheck in MODE by default, if `syntax-checking-enable-by-default' is
true."
  (when (and syntax-checking-enable-by-default
             (listp flycheck-global-modes)
             (not (eq 'not (car flycheck-global-modes))))
    (add-to-list 'flycheck-global-modes mode)))

(defun spacemacs/toggle-flycheck-error-list ()
  "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
  (interactive)
  (-if-let (window (flycheck-get-error-list-window))
      (quit-window nil window)
    (flycheck-list-errors)))

(defun spacemacs/goto-flycheck-error-list ()
  "Open and go to the error list buffer."
  (interactive)
  (unless (get-buffer-window (get-buffer flycheck-error-list-buffer))
    (flycheck-list-errors)
    (switch-to-buffer-other-window flycheck-error-list-buffer)))

(use-package flycheck
  :straight t
  :defer t
  :diminish flycheck-mode " ⓢ"
  :init
  (setq flycheck-standard-error-navigation nil
        flycheck-global-modes nil)
  (yq/add-toggle syntax-checking
    :mode flycheck-mode)
  (spacemacs/set-leader-keys "ts" 'yq/toggle-syntax-checking)
  (global-flycheck-mode 1)

  ;; Custom fringe indicator
  (define-fringe-bitmap 'my-flycheck-fringe-indicator
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00011100
            #b00111110
            #b00111110
            #b00111110
            #b00011100
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))

  (let ((bitmap 'my-flycheck-fringe-indicator))
    (flycheck-define-error-level 'error
      :severity 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap bitmap
      :fringe-face 'flycheck-fringe-error)
    (flycheck-define-error-level 'warning
      :severity 1
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap bitmap
      :fringe-face 'flycheck-fringe-warning)
    (flycheck-define-error-level 'info
      :severity 0
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap bitmap
      :fringe-face 'flycheck-fringe-info))
  (defun spacemacs/error-delegate ()
    "Decide which error API to delegate to.

Delegates to flycheck if it is enabled and the next-error buffer
is not visible. Otherwise delegates to regular Emacs next-error."
    (if (and (bound-and-true-p flycheck-mode)
             (let ((buf (ignore-errors (next-error-find-buffer))))
               (not (and buf (get-buffer-window buf)))))
        'flycheck
      'emacs))

  (defun spacemacs/next-error (&optional n reset)
    "Dispatch to flycheck or standard emacs error."
    (interactive "P")
    (let ((sys (spacemacs/error-delegate)))
      (cond
       ((eq 'flycheck sys) (call-interactively 'flycheck-next-error))
       ((eq 'emacs sys) (call-interactively 'next-error)))))

  (defun spacemacs/previous-error (&optional n reset)
    "Dispatch to flycheck or standard emacs error."
    (interactive "P")
    (let ((sys (spacemacs/error-delegate)))
      (cond
       ((eq 'flycheck sys) (call-interactively 'flycheck-previous-error))
       ((eq 'emacs sys) (call-interactively 'previous-error)))))

  (define-key flycheck-error-list-mode-map (kbd "j") #'next-line)
  (define-key flycheck-error-list-mode-map (kbd "k") #'previous-line)
  (add-to-list 'evil-insert-state-modes 'flycheck-error-list-mode)

  (push '("^\\*Flycheck.+\\*$"
          :regexp t
          :dedicated t
          :position bottom
          :stick t
          :noselect t)
        popwin:special-display-config)
  (spacemacs/set-leader-keys
    "eb" 'flycheck-buffer
    "ec" 'flycheck-clear
    "eh" 'flycheck-describe-checker
    "el" 'spacemacs/toggle-flycheck-error-list
    "eL" 'spacemacs/goto-flycheck-error-list
    "es" 'flycheck-select-checker
    "eS" 'flycheck-set-checker-executable
    "ev" 'flycheck-verify-setup
    "ex" 'flycheck-explain-error-at-point
    "en" 'spacemacs/next-error
    "ep" 'spacemacs/previous-error
    ))

(use-package smartparens
  :straight t
  :diminish smartparens-mode
  :config
  (smartparens-global-mode t)
  (define-key evil-normal-state-map "sd" 'sp-kill-sexp)
  (use-package smartparens-config))

(use-package yasnippet
  :straight t
  :diminish yas-global-mode
  :diminish yas-minor-mode
  :commands (yas-global-mode yas-minor-mode)
  :init
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (setq yas-triggers-in-field t
        yas-wrap-around-region t)
  (setq yas-prompt-functions '(yas-completing-prompt))
  (setq yas-minor-mode-map (make-sparse-keymap))
  (define-key yas-minor-mode-map (kbd "M-s-/") 'yas-next-field)
  :config
  (push 'yas-hippie-try-expand hippie-expand-try-functions-list)
  (push 'yas-installed-snippets-dir yas-snippet-dirs))

(use-package ediff
  :defer t
  :init
  ;; first we set some sane defaults
  (setq-default
   ediff-window-setup-function 'ediff-setup-windows-plain
   ;; emacs is evil and decrees that vertical shall henceforth be horizontal
   ediff-split-window-function 'split-window-horizontally
   ediff-merge-split-window-function 'split-window-horizontally)
  ;; (add-hook 'ediff-prepare-buffer-hook #'show-all)
  ;; restore window layout when done
  (add-hook 'ediff-quit-hook #'winner-undo))

(use-package dumb-jump
  :straight t
  :config
  (spacemacs/set-leader-keys "jq" #'dumb-jump-quick-look)
  (define-key evil-normal-state-map "gl" 'dumb-jump-go)
  (define-key evil-normal-state-map "gL" 'dumb-jump-go-other-window)
  (setq dumb-jump-prefer-searcher 'rg)
  (setq dumb-jump-force-searcher 'rg)
  (setq dumb-jump-selector 'ivy)
  ;; Since it's dumb, we add it to the end of the default jump handlers. At
  ;; the time of writing it is the only default jump handler. (gtags remains
  ;; mode-local)
  (add-to-list 'spacemacs-default-jump-handlers 'dumb-jump-go 'append))

;; TODO: auto-yas
