(yq/get-modules "lang/js2-imenu.el")
(spacemacs|define-jump-handlers js2-mode)
(setq js-indent-level 2)
(use-package js2-mode
  :straight t
  :mode "\\.js\\'"
  :diminish (js2-mode . "JS")
  :init
  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
  :config
  (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS")))
  ;; @see https://github.com/mooz/js2-mode/issues/350
  (setq forward-sexp-function nil)
  (setq js2-mode-show-parse-errors t)
  (setq js2-mode-show-strict-warnings nil)
  (evil-define-key 'normal js2-mode-map "," nil)
  (evil-define-key 'normal js2-mode-map ",d" nil)
  (evil-define-key 'normal js2-mode-map ",zc" 'js2-mode-hide-element)
  (evil-define-key 'normal js2-mode-map ",zo" 'js2-mode-show-element)
  (evil-define-key 'normal js2-mode-map ",zr" 'js2-mode-show-all)
  (evil-define-key 'normal js2-mode-map ",ze" 'js2-mode-toggle-element)
  (evil-define-key 'normal js2-mode-map ",zf" 'js2-mode-toggle-hide-functions)
  (evil-define-key 'normal js2-mode-map ",zC" 'js2-mode-toggle-hide-comments)
  (evil-define-key 'normal js2-mode-map ",w" 'js2-mode-toggle-warnings-and-errors))

(use-package js-doc
  :straight t
  :commands (js-doc-insert-file-doc js-doc-insert-function-doc js-doc-insert-tag js-doc-describe-tag):init
  (evil-define-key 'normal js2-mode-map ",db" 'js-doc-insert-file-doc)
  (evil-define-key 'normal js2-mode-map ",df" 'js-doc-insert-function-doc)
  (evil-define-key 'normal js2-mode-map ",df" 'js-doc-insert-tag)
  (evil-define-key 'normal js2-mode-map ",df" 'js-doc-describe-tag))

(defun spacemacs//tern-detect ()
  "Detect tern binary and warn if not found."
  (let ((found (executable-find "tern")))
    (unless found
      (message "tern binary not found!"))
    found))
(defun yq//set-tern-key-bindings (mode)
  "Set the key bindings for tern and the given MODE."
  (add-to-list 'tern-command "--no-port-file" 'append)
  (add-to-list (intern (format "spacemacs-jump-handlers-%S" mode))
               '(tern-find-definition :async t))
  (evil-define-key 'normal js2-mode-map ",t" nil)
  (evil-define-key 'normal js2-mode-map ",tf" 'tern-find-definition)
  (evil-define-key 'normal js2-mode-map ",tr" 'tern-rename-variable)
  (evil-define-key 'normal js2-mode-map ",td" 'tern-get-docs)
  (evil-define-key 'normal js2-mode-map ",tn" 'tern-find-definition-by-name)
  (evil-define-key 'normal js2-mode-map ",tp" 'tern-pop-find-definition)
  (evil-define-key 'normal js2-mode-map ",tt" 'tern-get-type))
(use-package tern
  :defer t
  :commands (tern-mode)
  :diminish tern-mode
  :init
  (add-hook 'js2-mode-hook 'tern-mode)
  (spacemacs//tern-detect)
  :config
  (add-to-list 'tern-command "--no-port-file" 'append)
  (yq//set-tern-key-bindings 'js2-mode)
  (dolist (mode '(js2-mode json-mode))
    (spacemacs/enable-flycheck mode)))

(use-package company-tern
  :straight t
  :after js2-mode
  :init
  (spacemacs|add-company-backends
    :backends company-tern
    :modes js2-mode))

(use-package json-mode
  :straight t
  :init
  :mode(("\\.json\\'" . json-mode)
        ("\\manifest.webapp\\'" . json-mode)
        ("\\.eslintrc\\'" . json-mode)
        ("\\.tern-project\\'" . json-mode)))

;; (bound-and-true-p prettier-js-mode)
(use-package prettier-js
  :straight t
  :diminish prettier-js-mode
  :commands (prettier-js-mode prettier-js)
  :hook (js2-mode . prettier-js-mode)
  :hook (rjsx-mode . prettier-js-mode)
  :hook (typescript-mode . prettier-js-mode)
  :config
  (yq/add-toggle prettier-js :mode prettier-js-mode)
  (evil-define-key 'normal js2-mode-map ",=" 'prettier-js)
  (evil-define-key 'normal js2-mode-map ",tp" 'yq/toggle-prettier-js))

(use-package rjsx-mode
  :straight t
  :defer t
  :mode (("\\.jsx\\'" . rjsx-mode))
  :commands (rjsx-delete-creates-full-tag rjsx-electric-gt rjsx-electric-lt rjsx-rename-tag-at-point)
  :init
  (add-to-list 'auto-mode-alist '("components\/.*\.js\'" . rjsx-mode))
  :config (progn (evil-define-key 'insert rjsx-mode-map (kbd "C-d") 'rjsx-delete-creates-full-tag
                   (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode "=" 'prettier-js)
                   (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode "m" 'js2-mode)))
  :commands (rjsx-mode))

(use-package js2-refactor
  :straight t
  :after js2-mode
  :commands (js2r-inline-var
             js2r-rename-var
             js2r-var-to-this
             js2r-ternary-to-if
             js2r-log-this
             js2r-kill
             js2r-toggle-function-async
             js2r-expand-node-at-point
             js2r--expand-contract-node-at-point)
  :init
  (evil-define-key 'normal js2-mode-map ",iv" 'js2r-inline-var)
  (evil-define-key 'normal js2-mode-map ",rv" 'js2r-rename-var)
  (evil-define-key 'normal js2-mode-map ",vt" 'js2r-var-to-this)
  (evil-define-key 'normal js2-mode-map ",3i" 'js2r-ternary-to-if)
  (evil-define-key 'normal js2-mode-map ",c" 'js2r-log-this)
  (evil-define-key 'normal js2-mode-map ",k" 'js2r-kill)
  (evil-define-key 'normal js2-mode-map ",ta" 'js2r-toggle-function-async)
  (evil-define-key 'normal js2-mode-map ",ep" 'js2r-expand-node-at-point)
  (evil-define-key 'normal js2-mode-map ",ec" 'js2r--expand-contract-node-at-point))

(use-package indium
  :straight t
  :after js2-mode
  :diminish (indium-interaction-mode . "In")
  :hook (js2-mode . indium-interaction-mode)
  :commands (indium-launch indium-interaction-mode indium-run-node indium-run-chrome indium-debugger-mode)
  :init (setq indium-nodejs-inspect-brk t)
  (add-to-list 'evil-insert-state-modes 'indium-repl-mode)
  (setq indium-debugger-inspect-when-eval t)
  (setq indium-chrome-executable "/Applications/Google Chrome Canary.app/Contents/MacOS/Google Chrome Canary")

  ;; launch indium
  (evil-define-key 'normal js2-mode-map ",il" 'indium-launch)
  (evil-define-key 'normal js2-mode-map ",ic" 'indium-connect)
  (evil-define-key 'normal js2-mode-map ",iq" 'indium-quit)
  :config
  ;; indium debugger mode
  (define-key indium-debugger-mode-map "h" nil)
  (define-key indium-debugger-mode-map "l" nil)
  (define-key indium-debugger-mode-map "s" nil)
  (define-key indium-debugger-mode-map "n" nil)
  (define-key indium-debugger-mode-map "p" nil)
  (define-key indium-debugger-mode-map "e" nil)
  (define-key indium-debugger-mode-map " " nil)
  (define-key indium-interaction-mode-map (kbd "C-c C-v") 'indium-switch-to-debugger)
  (define-key indium-repl-mode-map (kbd "C-c C-v") 'indium-switch-to-debugger)

  (evil-define-key 'normal indium-debugger-locals-mode-map "f" 'next-buffer)
  (evil-define-key 'normal indium-debugger-locals-mode-map "b" 'previous-buffer)
  (evil-define-key 'normal indium-debugger-locals-mode-map "q" 'quit-window)
  (evil-define-key 'normal indium-debugger-frames-mode-map "q" 'quit-window)

  (evil-define-key 'normal indium-repl-mode-map (kbd "C-a") 'evil-first-non-blank)
  (evil-define-key 'normal indium-repl-mode-map (kbd "C-e") 'evil-end-of-line)
  (evil-define-key 'insert indium-repl-mode-map (kbd "C-p") 'indium-repl-previous-input)
  (evil-define-key 'insert indium-repl-mode-map (kbd "C-n") 'indium-repl-next-input)

  ;; inspector
  (evil-define-key 'normal indium-inspector-mode-map "l" nil)
  (evil-define-key 'normal indium-inspector-mode-map "g" nil)
  (evil-define-key 'normal indium-inspector-mode-map "f" 'next-buffer)
  (evil-define-key 'normal indium-inspector-mode-map "b" 'previous-buffer)
  (evil-define-key 'normal indium-inspector-mode-map "q" 'quit-window)
  (evil-define-key 'normal indium-inspector-mode-map "u" 'indium-inspector-refresh)
  (evil-define-key 'normal indium-inspector-mode-map "o" 'indium-inspector-pop)
  (evil-define-key 'normal indium-inspector-mode-map "n" 'indium-inspector-next-reference)
  (evil-define-key 'normal indium-inspector-mode-map "p" 'indium-inspector-previous-reference)

  ;; make intercept enable automatically
  ;; (advice-add 'indium-debugger-mode :after (lambda (c) (evil-emacs-state) (evil-exit-emacs-state)))
  ;; (evil-make-intercept-map indium-debugger-mode-map)

  (evil-define-key 'normal js2-mode-map ",ee" 'indium-inspect-expression)
  (evil-define-key 'normal js2-mode-map ",eb" 'indium-eval-buffer)
  (evil-define-key 'normal js2-mode-map ",er" 'indium-eval-region)
  (evil-define-key 'normal js2-mode-map ",ef" 'indium-eval-defun)
  (evil-define-key 'normal js2-mode-map ",bb" 'indium-add-breakpoint)
  (evil-define-key 'normal js2-mode-map ",bc" 'indium-add-conditional-breakpoint)
  (evil-define-key 'normal js2-mode-map ",bu" 'indium-remove-breakpoint)
  (evil-define-key 'normal js2-mode-map ",bU" 'indium-remove-all-breakpoints-from-buffer)
  (evil-define-key 'normal js2-mode-map ",be" 'indium-edit-breakpoint-condition)
  (evil-define-key 'normal js2-mode-map ",bl" 'indium-list-breakpoints)
  (evil-define-key 'normal js2-mode-map ",bd" 'indium-deactivate-breakpoints)
  (evil-define-key 'normal js2-mode-map ",bd" 'indium-activate-breakpoints)
  (define-key js2-mode-map (kbd "C-c :") 'indium-inspect-expression)

  (defhydra hydra-indium (:hint nil :color pink :foreign-keys run)
    "
Breakpoint  | _b_ add   _u_ remove  _t_oggle
            | _l_ist _C_ondition _E_dit
Debug       | _H_ere  step _i_n/_o_ut _SPC_ step over _c_ontinue _e_/_E_valuate
Others      | _s_tack _n_ext/_p_rev stack _l_ocal _r_eload
    "
    ("b" indium-add-breakpoint)
    ("u" indium-remove-breakpoint)
    ("U" indium-remove-all-breakpoints-from-buffer)
    ("t" indium-toggle-breakpoint)
    ;; ("a" indium-activate-breakpoints)
    ;; ("d" indium-deactivate-breakpoints)
    ("C" indium-add-conditional-breakpoint)
    ("E" indium-edit-breakpoint-condition)
    ("H" indium-debugger-here)
    ("i" indium-debugger-step-into)
    ("o" indium-debugger-step-out)
    ("SPC" indium-debugger-step-over)
    ("c" indium-debugger-resume)
    ;; ("s" indium-debugger-stack-frames :exit t)
    ("s" indium-debugger-stack-frames)
    ("n" indium-debugger-next-frame)
    ("p" indium-debugger-previous-frame)
    ("E" indium-inspect-expression)
    ("e" indium-inspect-last-node)
    ("r" indium-reload)
    ;; ("l" indium-debugger-locals :exit t)
    ("l" indium-debugger-locals)
    (",," (lambda ()(interactive)) :exit t)
    ("q" (lambda ()(interactive)) :exit t))
  (evil-define-key 'normal js2-mode-map ",," 'hydra-indium/body)
  ;; (add-hook 'indium-script-parsed-hook (lambda (_) (interactive) (hydra-indium/body)))
  (add-hook 'indium-debugger-locals-mode-hook 'hydra-indium/lambda-q-and-exit)
  (add-hook 'indium-inspector-mode-hook 'hydra-indium/lambda-q-and-exit)
  (add-hook 'indium-debugger-frames-mode-hook 'hydra-indium/lambda-q-and-exit))
;; webpack config
;; output : {
;;   devtoolModuleFilenameTemplate: '[absolute-resource-path]',
;;   devtoolFallbackModuleFilenameTemplate: '[absolute-resource-path]?[hash]'
;; }