(defvar company-mode-completion-cancel-keywords
  '("do" "then" "begin" "case")
  "Keywords on which to cancel completion so that you can use RET
to complet without blocking common line endings.")

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
         ;; (backends (remove #'company-tabnine backends))
         ;; (backends (add-to-list 'backends #'company-tabnine t))
         (modes (spacemacs/mplist-get-values props :modes))
         (variables (spacemacs/mplist-get-values props :variables))
         (from (spacemacs/mplist-get-value props :from))
         (hooks (if (memq :append-hooks props)
                    (spacemacs/mplist-get-value props :append-hooks)
                  t))
         (call-hooks (when (memq :call-hooks props)
                       (spacemacs/mplist-get-value props :call-hooks)))
         (after-hook (spacemacs/mplist-get-value props :after-hook))
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
        ;; (push `(spacemacs|after-add-company-backends-hook) result)
        (push `(defvar ,raw-backends-var-name
                 spacemacs-default-company-backends
                 ,(format "Company backend list for %S." mode)) result)
        (push `(defvar ,backends-var-name ,raw-backends-var-name
                 ,(format "Company backend list for %S." mode)) result)
        ;; add backends
        (dolist (backend backends)
          (push `(add-to-list ',raw-backends-var-name ',backend) result))
        ;; (push `(add-to-list ',raw-backends-var-name #'company-tabnine) result))
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

(setq +company-default-idle-delay 0.2)
(defvar yq//company-numbers '(59 ?a ?s ?d ?f ?g ?h ?j ?k ?l))
(defun yq//company-format-numbers (numbered)
  (format " %s" (char-to-string (nth (mod numbered 10) yq//company-numbers))))
(defun spacemacs//company-transformer-cancel (candidates)
  "Cancel completion if prefix is in the list `company-mode-completion-cancel-keywords'"
  (unless (member company-prefix company-mode-completion-cancel-keywords)
    candidates))

(use-package company
  :straight t
  :diminish company-mode
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay +company-default-idle-delay)
  (company-selection-wrap-around t)
  (company-show-numbers t)
  (company-minimum-prefix-length 1)
  (company-require-match nil)
  (company-dabbrev-ignore-case t)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above nil)
  (company-dabbrev-downcase nil)
  (company-dabbrev-minimum-length 2)
  (company-dabbrev-time-limit 1)
  (company-echo-delay 0.2)
  (company-dabbrev-code-everywhere t)
  (company-dabbrev-code-other-buffers 'all)
  (company-dabbrev-code-time-limit 1)
  (company-search-regexp-function 'company-search-flex-regexp)
  (company-show-numbers-function 'yq//company-format-numbers)
  ;; (company-transformers '(spacemacs//company-transformer-cancel company-sort-by-occurrence)) ; lag
  (company-transformers '(spacemacs//company-transformer-cancel))
  (company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend))
  :init
  (with-eval-after-load 'fci-mode
    (defvar-local company-fci-mode-on-p nil)

    (defun company-turn-off-fci (&rest ignore)
      (when (boundp 'fci-mode)
        (setq company-fci-mode-on-p fci-mode)
        (when fci-mode (fci-mode -1))))

    (defun company-maybe-turn-on-fci (&rest ignore)
      (when company-fci-mode-on-p (fci-mode 1)))

    (add-hook 'company-completion-started-hook 'company-turn-off-fci)
    (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
    (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci))

  (defun +bind-company-active-map-key (_)
    (interactive)
    (evil-local-set-key 'insert (kbd "C-j") #'company-select-next)
    (evil-local-set-key 'insert (kbd "C-k") #'company-select-previous)
    (evil-local-set-key 'insert (kbd "C-r") #'company-show-doc-buffer)
    (evil-local-set-key 'insert (kbd "RET") #'company-complete-selection)
    (evil-local-set-key 'insert (kbd "C-l") #'company-complete-selection))

  (defun +unbind-company-active-map-key (_)
    (interactive)
    (evil-local-set-key 'insert (kbd "C-j") nil)
    (evil-local-set-key 'insert (kbd "C-k") nil)
    (evil-local-set-key 'insert (kbd "C-r") nil)
    (evil-local-set-key 'insert (kbd "RET") nil)
    (evil-local-set-key 'insert (kbd "C-l") nil))

  (add-hook 'company-completion-started-hook '+bind-company-active-map-key)
  (add-hook 'company-completion-finished-hook '+unbind-company-active-map-key)
  (add-hook 'company-completion-cancelled-hook '+unbind-company-active-map-key)

  :config
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (company-tng-configure-default)
  (setq company-active-map
        (let ((keymap (make-sparse-keymap)))
          (define-key keymap "\e\e\e" 'company-abort)
          (define-key keymap "\C-g" 'company-abort)
          (define-key keymap (kbd "C-j") 'company-select-next)
          (define-key keymap (kbd "C-k") 'company-select-previous)
          (define-key keymap (kbd "<down>") 'company-select-next-or-abort)
          (define-key keymap (kbd "<up>") 'company-select-previous-or-abort)
          (define-key keymap [remap scroll-up-command] 'company-next-page)
          (define-key keymap [remap scroll-down-command] 'company-previous-page)
          (define-key keymap [down-mouse-1] 'ignore)
          (define-key keymap [down-mouse-3] 'ignore)
          (define-key keymap [mouse-1] 'company-complete-mouse)
          (define-key keymap [mouse-3] 'company-select-mouse)
          (define-key keymap [up-mouse-1] 'ignore)
          (define-key keymap [up-mouse-3] 'ignore)
          (define-key keymap [return] #'newline-and-indent)
          (define-key keymap (kbd "C-l") 'company-complete-selection)
          (define-key keymap (kbd "C-m") #'newline-and-indent)
          (define-key keymap (kbd "RET") #'newline-and-indent)
          (define-key keymap [tab] 'company-complete-common)
          ;; (define-key keymap (kbd "TAB") 'company-complete-common-or-cycle)
          (define-key keymap (kbd "<f1>") 'company-show-doc-buffer)
          (define-key keymap (kbd "C-r") 'company-show-doc-buffer)
          ;; (define-key keymap "\C-w" 'company-show-location)
          (define-key keymap "\C-s" 'company-search-candidates)
          (define-key keymap "\C-\M-s" 'company-filter-candidates)
          (dotimes (i 10)
            (define-key keymap (read-kbd-macro (format "M-%d" i)) 'company-complete-number))
          (dotimes (i 10)
            (define-key keymap (read-kbd-macro (format "C-x C-6 %d" i)) 'company-complete-number))
          keymap)))

(use-package company-statistics
  :straight t
  :hook ((clojure-mode emacs-lisp-mode ) . company-statistics-mode))

(use-package company-flx
  :straight t
  :hook ((clojure-mode emacs-lisp-mode) . company-flx-mode))

(use-package company-tabnine
  :straight t
  :after company
  :custom
  (company-tabnine-binaries-folder "~/.TabNine/binaries/")
  :init
  ;; Use the tab-and-go frontend.
  ;; Allows TAB to select and complete at the same time.
  (setq company-tabnine-no-continue nil)
  (customize-set-variable 'company-backends '(company-tabnine
                                              company-capf
                                              (company-dabbrev-code
                                               company-keywords)
                                              company-files
                                              company-dabbrev))
  ;; https://github.com/TommyX12/company-tabnine/blob/master/README.md
  ;; workaround for company-transformers
  (setq company-tabnine--disable-next-transform nil)
  (defun my-company--transform-candidates (func &rest args)
    (if (not company-tabnine--disable-next-transform)
        (apply func args)
      (setq company-tabnine--disable-next-transform nil)
      (car args)))
  (defun my-company-tabnine (func &rest args)
    (when (eq (car args) 'candidates)
      (setq company-tabnine--disable-next-transform t))
    (apply func args))

  :config
  (advice-add #'company--transform-candidates :around #'my-company--transform-candidates)
  (advice-add #'company-tabnine :around #'my-company-tabnine)
  (defun yq-toggle-company-tabnine ()
    (interactive)
    (company-tabnine-restart-server)
    (if company-tabnine--disabled
        (progn
          (setq company-idle-delay 0)
          (setq company-tabnine--disabled nil)
          (message "Turn on company-tabnine"))
      (progn
        (setq company-idle-delay +company-default-idle-delay)
        (setq company-tabnine--disabled t)
        (message "Turn off company-tabnine"))))
  (setq company-tabnine--disabled t)
  (spacemacs/set-leader-keys "tt" 'yq-toggle-company-tabnine))

(use-package company-try-hard
  :straight t
  :after company
  :defer t
  :bind ((:map evil-insert-state-map) ("C-;" . company-try-hard)))

(use-package company-prescient
  :straight t
  :disabled
  :after (company prescient)
  :init (company-prescient-mode))

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
  :diminish flycheck-mode
  :custom
  (flycheck-check-syntax-automatically '(save idle-buffer-switch mode-enabled))
  (flycheck-standard-error-navigation nil)
  (flycheck-global-modes '(js2-mode rjsx-mode typescript-mode web-mode css-mode scss-mode json-mode))
  :init
  (yq/add-toggle syntax-checking :mode flycheck-mode)
  (spacemacs/set-leader-keys "ts" 'yq/toggle-syntax-checking)
  (setq flycheck-json-python-json-executable "/usr/local/bin/python3")

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

  (define-key evil-normal-state-map "]e" 'spacemacs/next-error)
  (define-key evil-normal-state-map "[e" 'spacemacs/previous-error)

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
    "ep" 'spacemacs/previous-error)
  (global-flycheck-mode 1)
  :config
  (define-key flycheck-error-list-mode-map "j" 'next-line)
  (define-key flycheck-error-list-mode-map "k" 'previous-line)
  (define-key flycheck-error-list-mode-map "q" 'quit-window)

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
      :fringe-face 'flycheck-fringe-info)))
  ;; (setq flycheck-javascript-eslint-executable "eslint_d"))

(use-package yasnippet
  :straight t
  :diminish yas-global-mode
  :diminish yas-minor-mode
  :commands (yas-global-mode yas-minor-mode)
  :hook (after-init . yas-global-mode)
  :custom
  (yas-triggers-in-field t)
  (yas-wrap-around-region t)
  (yas-prompt-functions '(yas-completing-prompt))
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
  (advice-add 'hippie-expand :after 'reenable-sp-hippie-advice
              ;; Set negative depth to make sure we go after
              ;; `sp-auto-complete-advice'.
              '((depth . -100)))

  (setq yas-minor-mode-map (make-sparse-keymap))
  (define-key yas-minor-mode-map (kbd "M-s-/") 'yas-next-field)
  :config
  (defun +yas-expand-when-inserting-dot (&optional args)
    (interactive)
    (if (eq major-mode 'vterm-mode) (vterm--self-insert)
      (if (eq (preceding-char) ?.)
          (if (and (not (delete-char -1 nil)) (yas-expand))
              t
            (progn
              (insert ?.)
              (insert ?.)))
        (insert ?.))))
  (define-key evil-insert-state-map "." '+yas-expand-when-inserting-dot)

  (push 'yas-hippie-try-expand hippie-expand-try-functions-list)
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  (yas-reload-all))

(use-package yasnippet-snippets
  :straight t
  :defer t
  :after (yasnippet))

(use-package smartparens
 :straight t
 :diminish smartparens-mode
 :commands (sp-kill-sexp sp-copy-sexp)
 :hook ((js2-mode rjsx-mode typescript-mode) . smartparens-strict-mode)
 :init
 (smartparens-global-mode t)
 (define-key yq-s-map "d" 'sp-kill-sexp)
 (define-key yq-s-map "," 'sp-copy-sexp)
 :config
 (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
 ;; (smartparens-global-strict-mode t)
 (defun yq/setup-sp-keys-for-lispy-modes (map)
   (evil-define-key 'normal map
    ;; "H" #'sp-previous-sexp
    ;; "L" #'sp-next-sexp
    "H" #'sp-backward-sexp
    "L" #'sp-forward-sexp
    "M" #'sp-mark-sexp))
 (require 'smartparens-config))

(use-feature ediff
  :defer t
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; emacs is evil and decrees that vertical shall henceforth be horizontal
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally)
  :init
  ;; restore window layout when done
  (add-hook 'ediff-quit-hook #'winner-undo))

(use-package dumb-jump
  :straight t
  :defer t
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-selector 'ivy)
  :bind
  (:map evil-normal-state-map
        ("gp" . #'dumb-jump-quick-look)
        ("gl" . #'dumb-jump-go)
        ("gL" . #'dumb-jump-go-other-window))
  :init
  (spacemacs/set-leader-keys "jq" #'dumb-jump-quick-look)
  ;; Since it's dumb, we add it to the end of the default jump handlers. At
  ;; the time of writing it is the only default jump handler. (gtags remains
  ;; mode-local)
  (add-to-list 'spacemacs-default-jump-handlers 'dumb-jump-go 'append)
  :config
  (add-to-list 'dumb-jump-project-denoters ".tabnine_root"))

(use-package eldoc
  :diminish eldoc-mode
  :commands (eldoc-mode)
  :hook ((cider-clojure-interaction-mode cider-repl-mode eval-expression-minibuffer-setup ielm-mode prog-mode) . eldoc-mode))

(use-package git-link
  :straight t
  :commands (git-link git-link-commit git-link-homepage)
  :custom
  (git-link-use-commit t)
  :init
  (spacemacs/set-leader-keys "kgc" #'git-link-commit)
  (spacemacs/set-leader-keys "kgh" #'git-link-homepage)
  (spacemacs/set-leader-keys "kgl" #'git-link)
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
               '("917\\.bimsop\\.com" git-link-commit-gogs)))

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(use-package hl-todo
  :straight t
  :hook (prog-mode . hl-todo-mode)
  :config
  (define-key hl-todo-mode-map (kbd "C-c C-t p") 'hl-todo-previous)
  (define-key hl-todo-mode-map (kbd "C-c C-t n") 'hl-todo-next)
  (define-key hl-todo-mode-map (kbd "C-c C-t o") 'hl-todo-occur))

(use-package rainbow-delimiters
  :straight t
  :hook ((ielm-mode emacs-lisp-mode clojure-mode cider-repl-mode rjsx-mode) . rainbow-delimiters-mode))

(with-eval-after-load 'hydra
  (defhydra hydra-change-mode (:hint nil :color pink)
    "
_e_  elisp    _c_  clojure   _t_  typescript
_j_  js2      _T_  text      _f_  fundamental
_g_  gfm      _m_ markdown
"
    ("e" emacs-lisp-mode :exit t)
    ("j" js2-mode :exit t)
    ("c" clojure-mode :exit t)
    ("T" text-mode :exit t)
    ("t" typescript-mode :exit t)
    ("f" fundamental-mode :exit t)
    ("m" markdown-mode :exit t)
    ("g" gfm-mode :exit t)
    ("q" hydra-keyboard-quit :exit t))
  (define-key yq-s-map (kbd "<RET>") 'hydra-change-mode/body))

(use-package ivy-xref
  :straight t
  :defer t
  :init
  (setq xref-prompt-for-identifier '(not xref-find-definitions
                                         xref-find-definitions-other-window
                                         xref-find-definitions-other-frame
                                         xref-find-references
                                         spacemacs/jump-to-definition))
  ;; Use ivy-xref to display `xref.el' results.
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(when (display-graphic-p)
  (use-package flycheck-posframe
    :straight t
    :after flycheck
    :diminish (flycheck-posframe-mode)
    :hook (flycheck-mode . flycheck-posframe-mode)
    :config (set-face-attribute 'flycheck-posframe-error-face nil :inherit 'error)))

(use-package repl-toggle
  :straight t
  :diminish repl-toggle-mode
  :custom
  (rtog/mode-repl-alist
   '((emacs-lisp-mode . ielm)
     (ruby-mode . inf-ruby)
     (enh-ruby-mode . inf-ruby)
     (js2-mode . switch-to-js)
     (rjsx-mode . switch-to-js)
     (typescript-mode . run-ts))
   rtog/fallback-repl-fun . projector-switch-to-or-create-project-shell)
  :hook ((ruby-mode enh-ruby-mode emacs-lisp-mode) . repl-toggle-mode))

(use-package corral
  :straight t
  :init
  (defun yq-setup-corral-keymap ()
    (evil-define-key 'insert (+major-mode-map) (kbd "M-9") #'corral-parentheses-backward)
    (evil-define-key 'insert (+major-mode-map) (kbd "M-0") #'corral-parentheses-forward)
    (evil-define-key 'insert (+major-mode-map) (kbd "M-]") #'corral-brackets-forward)
    (evil-define-key 'insert (+major-mode-map) (kbd "M-{") #'corral-braces-backward)
    (evil-define-key 'insert (+major-mode-map) (kbd "M-}") #'corral-braces-forward)
    (evil-define-key 'insert (+major-mode-map) (kbd "M-\"") #'corral-double-quotes-backward)
    (evil-define-key 'normal (+major-mode-map) (kbd "(") #'corral-parentheses-backward)
    (evil-define-key 'normal (+major-mode-map) (kbd ")") #'corral-parentheses-forward)
    (evil-define-key 'normal (+major-mode-map) (kbd "{") #'corral-braces-backward)
    (evil-define-key 'normal (+major-mode-map) (kbd "}") #'corral-braces-forward)
    (evil-define-key 'normal (+major-mode-map) (kbd "'") #'corral-double-quotes-backward))
  :hook ((js2-mode typescript-mode rjsx-mode) . #'yq-setup-corral-keymap))

(use-package request-deferred
  :straight t
  :defer t)

(use-package leetcode
  :straight (:host github :repo "kaiwk/leetcode.el")
  :disabled
  :custom
  (leetcode-prefer-language "javascript")
  :commands (leetcode)
  :config
  (evilified-state-evilify leetcode--problems-mode leetcode--problems-mode-map
    (kbd "RET") #'leetcode-show-descri
    "j" #'next-line
    "k" #'previous-line
    "r" #'leetcode-problems-refresh
    "q" #'quit-window))

(use-package dash-at-point
  :straight t
  :defer t
  :bind
  (:map evil-normal-state-map
        ("sa" . dash-at-point))
  :init
  (assq-delete-all 'clojure-mode dash-at-point-mode-alist)
  (add-to-list 'dash-at-point-mode-alist '(clojure-mode . "ClojureDocs")))

(use-package docker
  :straight t
  :defer t
  :commands (docker)
  :config
  (evilified-state-evilify docker-container-mode docker-container-mode-map
    "?" 'docker-container-help-popup
    "C" 'docker-container-cp-popup
    "D" 'docker-container-rm-popup
    "I" 'docker-container-inspect-popup
    "K" 'docker-container-kill-popup
    "L" 'docker-container-logs-popup
    "O" 'docker-container-stop-popup
    "P" 'docker-container-pause-popup
    "R" 'docker-container-restart-popup
    "S" 'docker-container-start-popup
    "a" 'docker-container-attach-popup
    "b" 'docker-container-shell-popup
    "d" 'docker-container-diff-popup
    "f" 'docker-container-find-file-popup
    "i" 'docker-container-ls-popup
    "r" 'docker-container-rename-selection))

;; (defvar hs-special-modes-alist
;;   (mapcar 'purecopy
;;           '((c-mode "{" "}" "/[*/]" nil nil)
;;             (c++-mode "{" "}" "/[*/]" nil nil)
;;             (bibtex-mode ("@\\S(*\\(\\s(\\)" 1))
;;             (java-mode "{" "}" "/[*/]" nil nil)
;;             (js-mode "{" "}" "/[*/]" nil)
;;             (rjsx-mode "`\\n\\|`[^;]\\|(\\|{\\|\\[" "`;\\|)\\|}\\|\\]" "/[*/]" nil))))

;; TODO
;; (add-to-list 'hs-special-modes-alist '(rjsx-mode "`\\n\\|`[^;]\\|(\\|{\\|\\[" "`;\\|)\\|}\\|\\]" "/[*/]" nil))

(use-package editorconfig
  :straight t
  :hook (prog-mode . editorconfig-mode))

(use-package imenu-list
  :straight t
  :defer t
  :bind ((:map yq-s-map ("L" . imenu-list-smart-toggle))))

(use-package copy-as-format
  :straight t
  :defer t
  :init
  (spacemacs/set-leader-keys "ccg" #'copy-as-format-github)
  (spacemacs/set-leader-keys "ccs" #'copy-as-format-slack)
  (spacemacs/set-leader-keys "cco" #'copy-as-format-org-mode)
  (spacemacs/set-leader-keys "ccm" #'copy-as-format-markdown))

;; (use-feature semantiic-mode
;;   :custom
;;   (semantic-default-submodes
;;    '(;; Use a database of parsed tags
;;      global-semanticdb-minor-mode
;;      ;; Perform semantic actions during idle time
;;      global-semantic-idle-scheduler-mode
;;      ;; Generate a summary of the current tag when idle
;;      global-semantic-idle-summary-mode
;;      ;; Decorate buffers with additional semantic information
;;      global-semantic-decoration-mode
;;      ;; Highlight the name of the function you're currently in
;;      global-semantic-highlight-func-mode
;;      ;; show the name of the function at the top in a sticky
;;      global-semantic-stickyfunc-mode
;;      ;; Show a breadcrumb of location during idle time
;;      global-semantic-idle-breadcrumbs-mode
;;      ;; Switch to recently changed tags with `semantic-mrub-switch-tags',
;;      ;; or `C-x B'
;;      global-semantic-mru-bookmark-mode
;;      global-semantic-idle-local-symbol-highlight-mode))
;;   :hook ((emacs-lisp-mode js2-mode rjsx-mode js-mode python-mode java-mode c-mode) . semantic-mode))

(use-package direnv
  :straight t
  :hook (after-init . direnv-mode)
  :custom
  (direnv-always-show-summary t)
  (direnv-show-paths-in-summary t)
  (direnv-use-faces-in-summary t))

(when (display-graphic-p)
  (use-package vterm
    :straight t
    :commands (vterm)
    :custom
    (vterm-kill-buffer-on-exit nil)
    :init
    (add-to-list 'evil-insert-state-modes #'vterm-mode)
    :config/el-patch
    (defun vterm--at-prompt-p ()
      "Return t if the cursor position is at shell prompt."
      (el-patch-swap (= (point) (or (vterm--get-prompt-point) 0))
                     (let ((p (point)))
                       (or (= p 0)
                           (< (abs (- (vterm--get-prompt-point) p)) 3)))))
    :config
    (setq vterm-keymp-exceptions nil)

    (defun tonic/maybe-kill-vterm (&rest n)
      (kill-buffer-and-window))

    (add-hook 'vterm-exit-functions #'tonic/maybe-kill-vterm)
    (define-key vterm-mode-map [return] #'vterm-send-return)
    (evil-define-key 'insert vterm-mode-map (kbd "C-e") #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-f") #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-a") #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-v") #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-b") #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-w") #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-u") #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-d") #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-n") #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-m") #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-p") #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-j") #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-k") #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-r") #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-t") #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-g") #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-c") #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-SPC") #'vterm--self-insert)
    (evil-define-key 'normal vterm-mode-map (kbd "C-d") #'vterm--self-insert)
    (evil-define-key 'normal vterm-mode-map (kbd "i") #'evil-insert-resume)
    (evil-define-key 'normal vterm-mode-map (kbd "o") #'evil-insert-resume)
    (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

  (use-package multi-vterm
    :straight t
    :disabled t
    :bind ("C-'" . +vterm-toggle)
    :config/el-patch
    (defun multi-vterm-projectile-get-buffer-name ()
      "Get projectile buffer name."
      (el-patch-swap (format "*vterm - %s*" (projectile-project-root))
                     (format "*vterm - %s*" (expand-file-name (projectile-project-root)))))
    :config
    (defun +vterm-toggle (&optional args)
      (interactive "p")
      (cond
       ((derived-mode-p 'vterm-mode)
        (multi-vterm-projectile))
       ((equal current-prefix-arg 1)
        (multi-vterm-projectile)
        (when (projectile-project-p)
          (setq vterm-toggle--cd-cmd
                (concat " cd " (shell-quote-argument (expand-file-name (projectile-project-root))))))
        (vterm-toggle-insert-cd))
       ((equal current-prefix-arg 2)
        (vterm-toggle-cd))
       (t
        (multi-vterm-projectile))))

    (defun +multi-vterm-projectile-background ()
      "Create new vterm buffer in the background."
      (print "+multi-vterm-projectile-background")
      (when (projectile-project-p)
          (when (not (buffer-live-p (get-buffer (multi-vterm-projectile-get-buffer-name))))
            (let* ((vterm-buffer (multi-vterm-get-buffer 'projectile))
                   (multi-vterm-buffer-list (nconc multi-vterm-buffer-list (list vterm-buffer))))
              (multi-vterm-internal)))))

    (add-hook 'find-file-hook '+multi-vterm-projectile-background))

  (use-package vterm-toggle
    :straight t
    :commands (vterm-toggle vterm-toggle-cd)
    :bind ("C-'" . vterm-toggle-cd)
    :custom
    (vterm-toggle-fullscreen-p nil)
    :init
    (push '("^\*?v?term.*" :regexp t :dedicated t :position bottom :stick t :height 0.4)
          popwin:special-display-config)))


;; TODO: explore verb
(use-package verb
  :straight t
  :disabled
  :mode ("\\.verb\\'" . verb-mode))

(use-package browse-at-remote
  :straight t
  :bind ("C-c g g" . browse-at-remote))

(use-package separedit
  :straight (:host github :repo "twlz0ne/separedit.el")
  :commands (separedit)
  :bind (:map prog-mode-map
              ("C-c '" . separedit)))

(use-package jq-mode
  :straight t
  :mode (("\\.jq$" . jq-mode))
  :init
  (with-eval-after-load 'json-mode
    (define-key json-mode-map (kbd "C-c C-j") #'jq-interactively)))

(use-package company-box
  :straight t
  ;; don't support capf
  :disabled t
  :hook (company-mode . company-box-mode))

(use-package 0xc
  :straight t
  :commands (0xc-convert 0xc-convert-point))