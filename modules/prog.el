(defvar auto-completion-enable-snippets-in-popup nil
  "If non nil show snippets in the auto-completion popup.")

(defun spacemacs//show-snippets-in-company (backend)
  (if (or (not auto-completion-enable-snippets-in-popup)
          (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(defvar spacemacs-default-company-backends
  '((company-dabbrev-code company-keywords
                          company-files company-dabbrev))
  "The list of default company backends used by spacemacs.
This variable is used to configure mode-specific company backends in spacemacs.
Backends in this list will always be active in these modes, as well as any
backends added by individual spacemacs layers.")

(defun spacemacs/mplist-get-values (plist prop)
  "Get the values associated to PROP in PLIST, a modified plist.

A modified plist is one where keys are keywords and values are
all non-keywords elements that follow it.

If there are multiple properties with the same keyword, only the first property
and its values is returned.

Currently this function infloops when the list is circular."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (pop tail))
    ;; pop the found keyword
    (pop tail)
    (while (and (consp tail) (not (keywordp (car tail))))
      (push (pop tail) result))
    (nreverse result)))

(defun spacemacs/mplist-get-value (plist prop)
  "Get a single value associated to PROP in PLIST, a modified plist.

You should always use this function instead of builtin `plist-get'
in Spacemacs.

A modified plist is one where keys are keywords and values are
all non-keywords elements that follow it.

If there are multiple properties with the same keyword, only the first property
and its values is returned.

Currently this function infloops when the list is circular."
  (car (spacemacs/mplist-get-values plist prop)))

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

`:append-hook BOOLEAN'
    Advanced property to control whether hooks functions are hooked or not,
    if non-nil hook functions are appended to modes hooks passed as `:modes'.

`:call-hooks BOOLEAN'
    if non-nil then hooked functions are called right away."
  (declare (indent 0))
  (let* ((backends (spacemacs/mplist-get-values props :backends))
         (backends (add-to-list 'backends #'company-tabnine))
         (modes (spacemacs/mplist-get-values props :modes))
         (variables (spacemacs/mplist-get-values props :variables))
         (from (spacemacs/mplist-get-value props :from))
         (hooks (if (memq :append-hooks props)
                    (spacemacs/mplist-get-value props :append-hooks)
                  t))
         (call-hooks (when (memq :call-hooks props)
                       (spacemacs/mplist-get-value props :call-hooks)))
         (result '(progn)))
    (dolist (mode modes)
      (let ((backends-var-name (intern (format "company-backends-%S" mode)))
            (raw-backends-var-name (intern (format "company-backends-%S-raw"
                                                   mode)))
            (init-func-name (intern (format "spacemacs//init-company-%S" mode)))
            (vars-func-name (intern
                             (format "spacemacs//init-company-vars-%S%s" mode
                                     (if from (format "-%S" from) ""))))
            (mode-hook-name (intern (format "%S-hook" mode))))
        ;; declare buffer local company-backends variable
        (push `(defvar ,raw-backends-var-name
                 spacemacs-default-company-backends
                 ,(format "Company backend list for %S." mode)) result)
        (push `(defvar ,backends-var-name ,raw-backends-var-name
                 ,(format "Company backend list for %S." mode)) result)
        ;; add backends
        (dolist (backend backends)
          (push `(add-to-list ',raw-backends-var-name ',backend) result))
        ;; define initialization hook function
        (push `(defun ,init-func-name ()
                 ,(format "Initialize company for %S." mode)
                 (if auto-completion-enable-snippets-in-popup
                     (setq ,backends-var-name
                           (mapcar 'spacemacs//show-snippets-in-company
                                   ,raw-backends-var-name))
                   (setq ,backends-var-name ,raw-backends-var-name))
                 (set (make-variable-buffer-local 'auto-completion-front-end)
                      'company)
                 (set (make-variable-buffer-local 'company-backends)
                      ,backends-var-name)) result)
        (when call-hooks
          (push `(,init-func-name) result))
        (when hooks
          (push `(add-hook ',mode-hook-name ',init-func-name t) result))
        ;; define variables hook function
        (when variables
          (let ((variables-copy variables)
                (vars-func `(defun ,vars-func-name ()
                              ,(format "Define company local variables for %S."
                                       mode)))
                vars)
            (while variables-copy
              (let* ((var (pop variables-copy))
                     (forms
                      (when (consp variables-copy)
                        `(set (make-variable-buffer-local ',var)
                              ,(eval (pop variables-copy))))))
                (when forms (push forms vars))))
            (push (append vars-func vars) result))
          (when call-hooks
            (push `(,vars-func-name) result))
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
        company-selection-wrap-around t
        company-show-numbers nil
        company-minimum-prefix-length 1
        company-require-match nil
        company-dabbrev-ignore-case t
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above nil
        company-dabbrev-downcase nil
        company-dabbrev-minimum-length 2
        company-dabbrev-time-limit 1
        company-dabbrev-code-everywhere t
        company-dabbrev-code-other-buffers 'all
        company-dabbrev-code-time-limit 1)
  (setq company-search-regexp-function 'company-search-flex-regexp)
  :config
  (setq company-backends '(company-capf
                           (company-dabbrev-code
                            company-gtags
                            company-etags
                            company-keywords)
                           company-files
                           company-dabbrev))
  (add-to-list 'company-frontends #'company-tng-frontend)
  (add-to-list 'company-frontends #'company-pseudo-tooltip-frontend)
  (add-to-list 'company-frontends #'company-echo-metadata-frontend)
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous)
  (evil-define-key 'insert company-active-map (kbd "C-j") 'company-select-next)
  (evil-define-key 'insert company-active-map (kbd "C-k") 'company-select-previous)
  (define-key company-active-map (kbd "C-l") 'company-complete-selection)
  (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
  (define-key company-active-map (kbd "C-d") 'company-show-location)
  (define-key company-active-map (kbd "C-m") 'newline-and-indent)
  (define-key company-active-map (kbd "C-r") 'company-show-doc-buffer)
  (evil-define-key 'insert company-active-map (kbd "C-r") 'company-show-doc-buffer)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-statistics
  :straight t
  :hook (company-mode . company-statistics-mode))

(use-package company-flx
  :straight t
  :commands (company-flx-mode)
  :init
  (company-flx-mode +1))

(use-package company-tabnine
  :straight t
  :init
  ;; Number the candidates (use M-1, M-2 etc to select completions).

  ;; Use the tab-and-go frontend.
  ;; Allows TAB to select and complete at the same time.
  (company-tng-configure-default)
  :config (add-to-list 'company-backends #'company-tabnine))

(use-package company-try-hard
  :straight t
  :after company
  :init
  (define-key evil-insert-state-map (kbd "C-;") 'company-try-hard))

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
  (setq flycheck-json-python-json-executable "/usr/local/bin/python3")

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

  (define-key evil-normal-state-map "]e" 'flycheck-next-error)
  (define-key evil-normal-state-map "[e" 'flycheck-previous-error)
  (define-key flycheck-error-list-mode-map "j" 'next-line)
  (define-key flycheck-error-list-mode-map "k" 'previous-line)
  (define-key flycheck-error-list-mode-map "q" 'quit-window)

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
    "eC" 'flycheck-select-checker
    "eS" 'flycheck-set-checker-executable
    "ev" 'flycheck-verify-setup
    "ex" 'flycheck-explain-error-at-point
    "en" 'spacemacs/next-error
    "ep" 'spacemacs/previous-error))

(use-package yasnippet
  :straight t
  :diminish yas-global-mode
  :diminish yas-minor-mode
  :commands (yas-global-mode yas-minor-mode)
  :init
  ;; https://github.com/joaotavora/yasnippet/issues/785
  (defvar smartparens-mode-original-value)
  (defun disable-sp-hippie-advice (&rest _)
    (setq smartparens-mode-original-value smartparens-mode)
    (setq smartparens-mode nil)
    t) ; We should still return t.
  ;; This advice could be added to other functions that usually insert
  ;; balanced parens, like `try-expand-list'.
  (advice-add 'yas-hippie-try-expand :after-while #'disable-sp-hippie-advice)

  (defun reenable-sp-hippie-advice (&rest _)
    (when (boundp 'smartparens-mode-original-value)
      (setq smartparens-mode smartparens-mode-original-value)
      (makunbound 'smartparens-mode-original-value)))
  (advice-add 'hippie-expand :after #'reenable-sp-hippie-advice
              ;; Set negative depth to make sure we go after
              ;; `sp-auto-complete-advice'.
              '((depth . -100)))

  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (setq yas-triggers-in-field t
        yas-wrap-around-region t)
  (setq yas-prompt-functions '(yas-completing-prompt))
  (setq yas-minor-mode-map (make-sparse-keymap))
  (define-key yas-minor-mode-map (kbd "M-s-/") 'yas-next-field)
  ;; (with-eval-after-load 'smartparens
  ;;   (add-hook 'yas-before-expand-snippet-hook
  ;;             #'spacemacs//smartparens-disable-before-expand-snippet)
  ;;   (add-hook 'yas-after-exit-snippet-hook
  ;;             #'spacemacs//smartparens-restore-after-exit-snippet))
  :config
  (setq yas-snippet-dirs '())
  (setq yas--default-user-snippets-dir (concat user-home-directory ".emacs.d/private/snippets/"))
  (push 'yas--default-user-snippets-dir yas-snippet-dirs)
  (push 'yas-hippie-try-expand hippie-expand-try-functions-list)
  ;; (add-hook 'snippet-mode 'yq/toggle-aggressive-indent-off)
  (unless (featurep 'warnings)
    (require 'warnings))
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  (yas-reload-all))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

(use-package smartparens
  :straight t
  :diminish smartparens-mode
  :config
  (smartparens-global-mode t)
  (define-key evil-normal-state-map "sd" 'sp-kill-sexp)
  (define-key evil-normal-state-map "s," 'sp-copy-sexp)
  (use-package smartparens-config))

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
  (define-key evil-normal-state-map "gp" #'dumb-jump-quick-look)
  (define-key evil-normal-state-map "gl" #'dumb-jump-go)
  (define-key evil-normal-state-map "gL" #'dumb-jump-go-other-window)
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

(use-package git-link
  :straight t
  :commands (git-link git-link-commit git-link-homepage)
  :config
  (defun git-link-gogs (hostname dirname filename branch commit start end)
    (format "http://%s/%s/src/%s/%s"
            hostname
            dirname
            (or branch commit)
            (concat filename
                    (when start
                      (concat "#"
                              (if end
                                  (format "L%s-L%s" start end)
                                (format "L%s" start)))))))
  (defun git-link-commit-gogs (hostname dirname commit)
    (format "http://%s/%s/commit/%s"
            hostname
            dirname
            commit))
  (add-to-list 'git-link-remote-alist
               '("917\\.bimsop\\.com" git-link-gogs))
  (add-to-list 'git-link-commit-remote-alist
               '("917\\.bimsop\\.com" git-link-commit-gogs))
  (setq git-link-open-in-browser t))
;; {{ shell and conf
(add-to-list 'auto-mode-alist '("\\.[^b][^a][a-zA-Z]*rc$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.aspell\\.en\\.pws\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\mimeapps\\.list$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.editorconfig$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.meta\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.?muttrc\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.mailcap\\'" . conf-mode))
;; }}

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; (use-package mixed-pitch
;;   :straight t
;;   :hook
;;   (org-mode . mixed-pitch-mode))

(use-package hl-todo
  :straight t
  :hook (prog-mode . hl-todo-mode))

(use-package rainbow-delimiters
  :straight t
  :defer t)

;; (use-package zop-to-char
;;   :straight t
;;   :init
;;   (evil-define-key '(normal insert) 'global (kbd "s-m") 'zop-up-to-char)
;;   (evil-define-key '(normal insert) 'global (kbd "s-M") 'zop-to-char))

(with-eval-after-load 'hydra
  (defhydra hydra-change-mode (:hint nil :color pink)
    "
_e_  elisp    _c_  clojure   _t_  typescript
_j_  js2      _T_     text   _f_  fundamental
"
    ("e" emacs-lisp-mode :exit t)
    ("j" js2-mode :exit t)
    ("c" clojure-mode :exit t)
    ("T" text-mode :exit t)
    ("t" typescript-mode :exit t)
    ("f" fundamental-mode :exit t)
    ("q" hydra-keyboard-quit :exit t))
  (define-key evil-normal-state-map (kbd "s <RET>") 'hydra-change-mode/body))

(use-package ivy-xref
  :straight t
  :init
  (setq xref-prompt-for-identifier '(not xref-find-definitions
                                         xref-find-definitions-other-window
                                         xref-find-definitions-other-frame
                                         xref-find-references
                                         spacemacs/jump-to-definition))
  ;; Use ivy-xref to display `xref.el' results.
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package all-the-icons
  :straight t)

(use-package flycheck-posframe
  :straight t
  :after flycheck
  :diminish (flycheck-posframe-mode)
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config (set-face-attribute 'flycheck-posframe-error-face nil :inherit 'error))
