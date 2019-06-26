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
(use-package company
  :straight t
  :diminish company-mode
  :init
  (setq company-idle-delay +company-default-idle-delay
        company-selection-wrap-around t
        company-show-numbers t
        company-minimum-prefix-length 1
        company-require-match nil
        company-dabbrev-ignore-case t
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above nil
        company-dabbrev-downcase nil
        company-dabbrev-minimum-length 2
        company-dabbrev-time-limit 1
        company-echo-delay 0.2
        company-dabbrev-code-everywhere t
        company-dabbrev-code-other-buffers 'all
        company-dabbrev-code-time-limit 1)
  (setq company-search-regexp-function 'company-search-flex-regexp)
  (customize-set-variable 'company-backends '(company-tabnine
                                              company-capf
                                              (company-dabbrev-code
                                               company-keywords)
                                              company-files
                                              company-dabbrev))
  (defvar yq//company-numbers '(59 ?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (defun yq//company-format-numbers (numbered)
    (format " %s" (char-to-string (nth (mod numbered 10) yq//company-numbers))))

  (global-company-mode)
  :config
  (defvar-local company-fci-mode-on-p nil)

  (defun company-turn-off-fci (&rest ignore)
    (when (boundp 'fci-mode)
      (setq company-fci-mode-on-p fci-mode)
      (when fci-mode (fci-mode -1))))

  (defun company-maybe-turn-on-fci (&rest ignore)
    (when company-fci-mode-on-p (fci-mode 1)))

  (add-hook 'company-completion-started-hook 'company-turn-off-fci)
  (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
  (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)

  (defun spacemacs//company-complete-common-or-cycle-backward ()
    "Complete common prefix or cycle backward."
    (interactive)
    (company-complete-common-or-cycle -1))

  (setq company-show-numbers-function 'yq//company-format-numbers)

  (defun spacemacs//company-transformer-cancel (candidates)
    "Cancel completion if prefix is in the list `company-mode-completion-cancel-keywords'"
    (unless (member company-prefix company-mode-completion-cancel-keywords)
      candidates))
  ;; lag
  ;; (setq company-transformers '(spacemacs//company-transformer-cancel company-sort-by-occurrence))
  (setq company-transformers '(spacemacs//company-transformer-cancel))
  ;; (add-to-list 'company-frontends #'company-tng-frontend)
  ;; (add-to-list 'company-frontends #'company-pseudo-tooltip-frontend)
  ;; (add-to-list 'company-frontends #'company-echo-metadata-frontend)

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
          (define-key keymap [return] 'company-complete-selection)
          (define-key keymap (kbd "C-m") #'newline-and-indent)
          (define-key keymap (kbd "C-l") 'company-complete-selection)
          (define-key keymap (kbd "RET") 'company-complete-selection)
          (define-key keymap [tab] 'company-complete-common)
          (define-key keymap (kbd "TAB") 'company-complete-common-or-cycle)
          (define-key company-active-map (kbd "<S-tab>")
            'spacemacs//company-complete-common-or-cycle-backward)
          (define-key company-active-map (kbd "<backtab>")
            'spacemacs//company-complete-common-or-cycle-backward)
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
  :hook (company-mode . company-statistics-mode))

(use-package company-flx
  :straight t
  :commands (company-flx-mode)
  :init
  (company-flx-mode +1))

(use-package company-tabnine
  :straight t
  :after company
  :init
  ;; Number the candidates (use M-1, M-2 etc to select completions).

  ;; Use the tab-and-go frontend.
  ;; Allows TAB to select and complete at the same time.
  (company-tng-configure-default)
  (setq company-tabnine-no-continue nil)
  :config
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
  ;; disable flycheck by default
  ;; (global-flycheck-mode 1)
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
  (defun +yas-expand-when-inserting-dot ()
    (interactive)
    (if (eq (preceding-char) ?.)
        (if (and (not (delete-char -1 nil)) (yas-expand))
            t
          (progn
            (insert ?.)
            (insert ?.)))
      (insert ?.)))

  (define-key evil-insert-state-map "." '+yas-expand-when-inserting-dot)

  (setq yas-snippet-dirs '())
  (setq yas--default-user-snippets-dir (concat user-home-directory ".emacs.d/snippets/"))
  (push 'yas--default-user-snippets-dir yas-snippet-dirs)
  (push 'yas-hippie-try-expand hippie-expand-try-functions-list)
  ;; (add-hook 'snippet-mode 'yq/toggle-aggressive-indent-off)
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  (yas-reload-all))

(use-package yasnippet-snippets
  :straight t)

(use-package smartparens
  :straight t
  :diminish smartparens-mode
  :config
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (smartparens-global-strict-mode t)
  (define-key yq-s-map "d" 'sp-kill-sexp)
  (define-key yq-s-map "," 'sp-copy-sexp)
  (defun yq/setup-sp-keys-for-lispy-modes (map)
    (evil-define-key 'normal map
      ;; "H" #'sp-previous-sexp
      ;; "L" #'sp-next-sexp
      "H" #'sp-backward-sexp
      "L" #'sp-forward-sexp
      "M" #'sp-mark-sexp))
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
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-selector 'ivy)
  :config
  (add-to-list 'dumb-jump-project-denoters ".tabnine_root")
  (spacemacs/set-leader-keys "jq" #'dumb-jump-quick-look)
  (define-key evil-normal-state-map "gp" #'dumb-jump-quick-look)
  (define-key evil-normal-state-map "gl" #'dumb-jump-go)
  (define-key evil-normal-state-map "gL" #'dumb-jump-go-other-window)
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

;; shell and conf
(add-to-list 'auto-mode-alist '("\\.[^b][^a][a-zA-Z]*rc$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.aspell\\.en\\.pws\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\mimeapps\\.list$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.editorconfig$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.meta\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.?muttrc\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.mailcap\\'" . conf-mode))

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(use-package hl-todo
  :straight t
  :hook (prog-mode . hl-todo-mode)
  :config
  (define-key hl-todo-mode-map (kbd "C-c p") 'hl-todo-previous)
  (define-key hl-todo-mode-map (kbd "C-c n") 'hl-todo-next)
  (define-key hl-todo-mode-map (kbd "C-c o") 'hl-todo-occur))

(use-package rainbow-delimiters
  :straight t
  :defer t)

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
  (define-key yq-s-map (kbd "<RET>") 'hydra-change-mode/body))

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

(use-package flycheck-posframe
  :straight t
  :after flycheck
  :diminish (flycheck-posframe-mode)
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config (set-face-attribute 'flycheck-posframe-error-face nil :inherit 'error))

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
  :config
  (repl-toggle-mode))

(use-package corral
  :straight t
  :init
  (defun yq-setup-corral-keymap ()
    (evil-define-key 'insert (+major-mode-map) (kbd "M-9") #'corral-parentheses-backward)
    (evil-define-key 'insert (+major-mode-map) (kbd "M-0") #'corral-parentheses-forward)
    (evil-define-key 'insert (+major-mode-map) (kbd "M-[") #'corral-brackets-backward)
    (evil-define-key 'insert (+major-mode-map) (kbd "M-]") #'corral-brackets-forward)
    (evil-define-key 'insert (+major-mode-map) (kbd "M-{") #'corral-braces-backward)
    (evil-define-key 'insert (+major-mode-map) (kbd "M-}") #'corral-braces-forward)
    (evil-define-key 'insert (+major-mode-map) (kbd "M-\"") #'corral-double-quotes-backward)
    (evil-define-key 'normal (+major-mode-map) (kbd "(") #'corral-parentheses-backward)
    (evil-define-key 'normal (+major-mode-map) (kbd ")") #'corral-parentheses-forward)
    (evil-define-key 'normal (+major-mode-map) (kbd "[") #'corral-brackets-backward)
    (evil-define-key 'normal (+major-mode-map) (kbd "]") #'corral-brackets-forward)
    (evil-define-key 'normal (+major-mode-map) (kbd "{") #'corral-braces-backward)
    (evil-define-key 'normal (+major-mode-map) (kbd "}") #'corral-braces-forward)
    (evil-define-key 'normal (+major-mode-map) (kbd "'") #'corral-double-quotes-backward))
  :hook ((js2-mode typescript-mode rjsx-mode) . #'yq-setup-corral-keymap))

(use-package request-deferred
  :straight t
  :defer t)

(use-package leetcode
  :straight (:host github :repo "kaiwk/leetcode.el")
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
  :bind
  (:map evil-normal-state-map
        ("sa" . dash-at-point))
  :init
  (assq-delete-all 'clojure-mode dash-at-point-mode-alist)
  (add-to-list 'dash-at-point-mode-alist '(clojure-mode . "ClojureDocs")))

(use-package docker
  :straight t
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