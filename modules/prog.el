(defvar auto-completion-enable-snippets-in-popup t
  "If non nil show snippets in the auto-completion popup.")

(defun spacemacs//show-snippets-in-company (backend)
  (if (or (not auto-completion-enable-snippets-in-popup)
          (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(defvar spacemacs-default-company-backends
  '((company-dabbrev-code company-gtags company-etags company-keywords)
    company-files company-dabbrev)
  "The list of default company backends used by spacemacs.
This variable is used to configure mode-specific company backends in spacemacs.
Backends in this list will always be active in these modes, as well as any
backends added by individual spacemacs layers.")

(defmacro spacemacs|add-company-backends (&rest props)
  "Add and enable company backends.
This function should be called exclusively in `post-init-company' functions or
`init-company-xxx' function where xxx is company backend package.

Available PROPS:

`:backends BACKENDS'
   One or several symbols or lists representing a company backend or a list of
   company backends.

`:modes MODES'
    One or several modes where BACKENDS will be added.

`:variables VAR VALUE'
    One or several VAR VALUE pairs (similar to layer variables).
    These variables are made buffer local so their values are set only for
    the given MODES.

`:from SYMBOL'
    Advanced property aimed at avoiding hook function name conflicts when
    `:variables' property is used in several calls to this macro for the same
    MODES.

`:hook BOOLEAN'
    Advanced property to control whether hooks functions are hooked or not,
    if non-nil hook functions are appended to modes hooks passed as `:modes'."
  (declare (indent 0))
  (let* ((backends (spacemacs/mplist-get props :backends))
         (modes (spacemacs/mplist-get props :modes))
         (variables (spacemacs/mplist-get props :variables))
         (from (plist-get props :from))
         (hooks (if (memq :hooks props)
                    (plist-get props :hooks)
                  t))
         (result '(progn)))
    (dolist (mode modes)
      (let ((backends-var-name (intern (format "company-backends-%S" mode)))
            (init-func-name (intern (format "spacemacs//init-company-%S" mode)))
            (vars-func-name (intern
                             (format "spacemacs//init-company-vars-%S%s" mode
                                     (if from (format "-%S" from) ""))))
            (mode-hook-name (intern (format "%S-hook" mode))))
        ;; declare buffer local company-backends variable
        (push `(defvar ,backends-var-name
                 spacemacs-default-company-backends
                 ,(format "Company backend list for %S." mode)) result)
        ;; add backends
        (dolist (backend backends)
          (push `(add-to-list ',backends-var-name ',backend) result))
        ;; define initialization hook function
        (push `(defun ,init-func-name ()
                 ,(format "Initialize company for %S." mode)
                 (when auto-completion-enable-snippets-in-popup
                   (setq ,backends-var-name
                         (mapcar 'spacemacs//show-snippets-in-company
                                 ,backends-var-name)))
                 (set (make-variable-buffer-local 'auto-completion-front-end)
                      'company)
                 (set (make-variable-buffer-local 'company-backends)
                      ,backends-var-name)) result)
        (when hooks
          (push `(add-hook ',mode-hook-name ',init-func-name t) result))
        ;; define variables hook function
        (when variables
          (let ((vars-func `(defun ,vars-func-name ()
                              ,(format "Define company local variables for %S."
                                       mode)))
                vars)
            (while variables
              (let* ((var (pop variables))
                     (forms
                      (when (consp variables)
                        `(set (make-variable-buffer-local ',var)
                              ,(eval (pop variables))))))
                (when forms (push forms vars))))
            (push (append vars-func vars) result))
          (when hooks
            (push `(add-hook ',mode-hook-name ',vars-func-name t) result)))
        (when hooks
          (push `(add-hook ',mode-hook-name 'company-mode t) result))))
    ;; return the expanded macro in correct order
    (reverse result)))

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

(use-package company-childframe
  :straight t
  :diminish company-childframe-mode
  :config
  (company-childframe-mode 1))

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
  (define-key evil-normal-state-map "s," 'sp-copy-sexp)
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
  (setq yas--default-user-snippets-dir (concat user-home-directory ".emacs.d/private/snippets/"))
  (push 'yas-installed-snippets-dir yas-snippet-dirs)
  (push 'yas--default-user-snippets-dir yas-snippet-dirs)
  (push 'yas-hippie-try-expand hippie-expand-try-functions-list))

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

(use-package eldoc
  :diminish eldoc-mode
  :commands (eldoc-mode)
  :hook (emacs-lisp-mode . eldoc-mode)
  :config
  ;; enable eldoc in `eval-expression'
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  ;; enable eldoc in IELM
  (add-hook 'ielm-mode-hook #'eldoc-mode))


;; TODO: auto-yas
