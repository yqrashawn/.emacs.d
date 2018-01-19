(use-package js2-mode
  :straight t
  :mode "\\.js\\'"
  :init
  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
  :config
  (evil-define-key 'normal 'js2-mode-map "," nil)
  (evil-define-key 'normal 'js2-mode-map ",d" nil)
  (evil-define-key 'normal 'js2-mode-map ",zc" 'js2-mode-hide-element)
  (evil-define-key 'normal 'js2-mode-map ",zo" 'js2-mode-show-element)
  (evil-define-key 'normal 'js2-mode-map ",zr" 'js2-mode-show-all)
  (evil-define-key 'normal 'js2-mode-map ",ze" 'js2-mode-toggle-element)
  (evil-define-key 'normal 'js2-mode-map ",zf" 'js2-mode-toggle-hide-functions)
  (evil-define-key 'normal 'js2-mode-map ",zC" 'js2-mode-toggle-hide-comments)
  (evil-define-key 'normal 'js2-mode-map ",w" 'js2-mode-toggle-warnings-and-errors))

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

(use-package tern
  :defer t
  :commands (tern-mode)
  :hook (js2-mode . tern-mode)
  :init
  (spacemacs//tern-detect)
  :config
  (add-to-list 'tern-command "--no-port-file" 'append)
  (evil-define-key 'normal 'js2-mode-map ",t" nil)
  (evil-define-key 'normal 'js2-mode-map ",tr" 'tern-rename-variable)
  (evil-define-key 'normal 'js2-mode-map ",td" 'tern-get-docs)
  (evil-define-key 'normal 'js2-mode-map ",tt" 'tern-get-type))

(use-package company-tern
  :straight t
  :init
  (add-to-list 'company-backends 'company-tern))

(use-package json-mode
  :straight t
  :init
  :mode "\\.json\\'")

(defun yq/toggle-prettier-js-mode ()
  (interactive)
  (if (bound-and-true-p prettier-js-mode)
      (prettier-js-mode -1)
    (prettier-js-mode 1)))

(use-package prettier-js
  :straight t
  :commands (prettier-js-mode prettier-js)
  :init
  (evil-define-key 'normal 'js2-mode-map ",=" 'prettier-js)
  (evil-define-key 'normal 'js2-mode-map ",tp" 'yq/toggle-prettier-js-mode))