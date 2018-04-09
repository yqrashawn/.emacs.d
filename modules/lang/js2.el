(yq/get-modules "lang/js2-imenu.el")
(spacemacs|define-jump-handlers js2-mode)
(setq js-indent-level 2)
(use-package js2-mode
  :straight t
  :mode "\\.js\\'"
  :init
  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
  :config
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
  :init
  (spacemacs|add-company-backends
    :backends company-tern
    :modes js2-mode))

(use-package json-mode
  :straight t
  :init
  :mode "\\.json\\'")

;; (bound-and-true-p prettier-js-mode)
(use-package prettier-js
  :straight t
  :diminish prettier-js-mode
  :commands (prettier-js-mode prettier-js)
  :init
  (yq/add-toggle prettier-js :mode prettier-js-mode)
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
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
  :defer t
  :init
  (evil-define-key 'normal js2-mode-map ",iv" 'js2r-inline-var)
  (evil-define-key 'normal js2-mode-map ",rv" 'js2r-rename-var)
  (evil-define-key 'normal js2-mode-map ",vt" 'js2r-var-to-this)
  (evil-define-key 'normal js2-mode-map ",3i" 'js2r-ternary-to-if)
  (evil-define-key 'normal js2-mode-map ",c" 'js2r-log-this)
  (evil-define-key 'normal js2-mode-map ",k" 'js2r-kill)
  (evil-define-key 'normal js2-mode-map ",ta" 'js2r-toggle-function-async)
  (evil-define-key 'normal js2-mode-map ",ee" 'js2r-expand-node-at-point)
  (evil-define-key 'normal js2-mode-map ",ec" 'js2r--expand-contract-node-at-point))