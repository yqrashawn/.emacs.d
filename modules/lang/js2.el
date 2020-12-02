;;; js2.el ---  javascript packages -*- lexical-binding: t; -*-

(yq/get-modules "lang/js2-imenu.el")
(spacemacs|define-jump-handlers js2-mode)
(spacemacs|define-jump-handlers typescript-mode)
(spacemacs|define-jump-handlers js-mode)
(spacemacs|define-jump-handlers rjsx-mode)
(spacemacs|define-jump-handlers json-mode)
(setq js-indent-level 2)
(use-package js2-mode
  :straight t
  ;; :mode "\\.js\\'"
  :diminish (js2-mode . "JS")
  ;; :hook (js-mode . js2-minor-mode)
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
  (evil-define-key 'normal js2-mode-map ",zc" #'js2-mode-hide-element)
  (evil-define-key 'normal js2-mode-map ",zo" #'js2-mode-show-element)
  (evil-define-key 'normal js2-mode-map ",zr" #'js2-mode-show-all)
  (evil-define-key 'normal js2-mode-map ",ze" #'js2-mode-toggle-element)
  (evil-define-key 'normal js2-mode-map ",zf" #'js2-mode-toggle-hide-functions)
  (evil-define-key 'normal js2-mode-map ",zC" #'js2-mode-toggle-hide-comments)
  (evil-define-key 'normal js2-mode-map ",w" #'js2-mode-toggle-warnings-and-errors))

(use-package js-doc
  :straight t
  :commands (js-doc-insert-file-doc js-doc-insert-function-doc js-doc-insert-tag js-doc-describe-tag):init
  (evil-define-key 'normal js2-mode-map ",db" #'js-doc-insert-file-doc)
  (evil-define-key 'normal js2-mode-map ",df" #'js-doc-insert-function-doc)
  (evil-define-key 'normal js2-mode-map ",df" #'js-doc-insert-tag)
  (evil-define-key 'normal js2-mode-map ",df" #'js-doc-describe-tag))

(defun spacemacs//tern-detect ()
  "Detect tern binary and warn if not found."
  (let ((found (executable-find "tern")))
    (unless found
      (message "tern binary not found!"))
    found))

(use-package tern
  :defer t
  :disabled
  :commands (tern-mode)
  :diminish tern-mode
  :hook ((js-mode js2-mode rjsx-mode) . tern-mode)
  :init
  (spacemacs//tern-detect)
  :config
  (add-to-list 'tern-command "--no-port-file" 'append)
  (dolist (mode '(js2-mode json-mode rjsx-mode typescript-mode))
    (let ((l (intern (format "spacemacs-jump-handlers-%S" mode))))
      (when (special-variable-p l) (add-to-list l'(tern-find-definition :async t))))
    (spacemacs/enable-flycheck mode)))

(use-package company-tern
  :straight t
  :disabled
  :after tern
  :init
  (spacemacs|add-company-backends
    :backends (company-tern)
    :modes (js-mode js2-mode rjsx-mode)
    :after-hook t))

(use-package json-mode
  :straight t
  :init
  :mode(("\\.json\\'" . json-mode)
        ("\\manifest.webapp\\'" . json-mode)
        ("\\.eslintrc\\'" . json-mode)
        ("\\.tern-project\\'" . json-mode)))

;; (bound-and-true-p prettier-js-mode)
(use-package rjsx-mode
  :straight t
  :defer t
  :mode (("\\.js\\'" . rjsx-mode) ("\\.jsx\\'" . rjsx-mode) ("components\\/.*\\.js\\'" . rjsx-mode))
  :commands (rjsx-delete-creates-full-tag rjsx-electric-gt rjsx-electric-lt rjsx-rename-tag-at-point)
  :config
  (defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
    "Workaround sgml-mode and follow airbnb component style."
    (save-excursion
      (beginning-of-line)
      (if (looking-at-p "^ +\/?> *$")
          (delete-char sgml-basic-offset))))
  (evil-define-key 'insert rjsx-mode-map (kbd "C-d") 'rjsx-delete-creates-full-tag)
  (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode "m" 'js2-mode))

(use-package prettier
  :straight t
  :hook (after-init . global-prettier-mode)
  :custom
  (prettier-editorconfig-flag t)
  :init
  (yq/add-toggle prettier :mode prettier-mode)
  (with-eval-after-load 'js2-mode
    (evil-define-key 'normal js2-mode-map (kbd ",=") #'prettier-prettify)
    (evil-define-key 'normal js2-mode-map ",tp" 'yq/toggle-prettier))
  (with-eval-after-load 'rjsx-mode
    (evil-define-key 'normal rjsx-mode-map (kbd ",=") #'prettier-prettify)
    (evil-define-key 'normal rjsx-mode-map ",tp" 'yq/toggle-prettier)))

(use-package js2-refactor
  :straight t
  :after (js2-mode rjsx-mode)
  :hook ((js2-mode rjsx-mode typescript-mode) . js2-refactor-mode)
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
  (js2r-add-keybindings-with-prefix "C-c m"))

(use-package add-node-modules-path
  :straight t
  :hook ((js-mode js2-mode rjsx-mode typescript-mode) . #'add-node-modules-path))

(use-package js-comint
  :straight t
  :disabled
  :commands (run-js switch-to-js)
  :init
  (setq js-program-command "node"
        js-program-arguments '("--interactive")
        js-comint-prompt "node > ")
  (evil-define-key 'normal  js2-mode-map  ",eb" #'js-send-buffer)
  (evil-define-key 'normal  js2-mode-map  ",ee" #'js-send-last-sexp)
  (evil-define-key 'normal  js2-mode-map  ",er" #'js-send-region)
  (evil-define-key 'normal  js2-mode-map  ",em" #'js-comint-add-module-path)
  (define-key js2-mode-map (kbd "C-x C-e") #'js-send-last-sexp)
  (define-key js2-mode-map (kbd "C-c b") #'js-send-buffer)
  (define-key js2-mode-map (kbd "C-c C-b") #'js-send-buffer-and-go)
  (define-key js2-mode-map (kbd "C-c C-z") #'rtog/toggle-repl)
  (define-key js2-mode-map (kbd "C-c C-l") #'js-comint-clear)
  :config
  (add-to-list 'js-comint-module-paths (expand-file-name "~/local/bin/node_modules"))
  (define-key js-comint-mode-map (kbd "C-c C-z") #'rtog/toggle-repl)
  (define-key js-comint-mode-map (kbd "C-c C-l") #'js-comint-clear)
  (defun inferior-js-mode-hook-setup ()
    (add-hook 'comint-output-filter-functions 'js-comint-process-output))
  (add-hook 'inferior-js-mode-hook 'inferior-js-mode-hook-setup t))

(dolist (mode '(js2-mode json-mode rjsx-mode typescript-mode))
  (spacemacs/enable-flycheck mode))

(use-package eslintd-fix
  :straight t
  :defer t
  ;; :hook (rjsx-mode . eslintd-fix-mode)
  :custom (eslintd-fix-executable "eslint_d"))

(use-package indium
  :straight t
  :disabled
  :commands (indium-run-node)
  :defer t)

(with-eval-after-load 'smartparens
  ;; Don't pair lifetime specifiers
  (dolist (mode '(js-mode js2-mode rjsx-mode typescript-mode))
    (sp-local-pair mode "{" nil :post-handlers '((+indent-between-pair "RET")))
    (sp-local-pair mode "[" nil :post-handlers '((+indent-between-pair "RET")))
    (sp-local-pair mode "(" nil :post-handlers '((+indent-between-pair "RET")))))

(use-package npm-mode
  :straight t
  :hook ((js-mode rjsx-mode js2-mode typescript-mode) . npm-mode)
  :init
  (push '("^\*yarn:.*" :regexp t :dedicated t :position bottom :stick t :height 0.4)
          popwin:special-display-config)
  :config/el-patch
  ;; npm -> yarn
  (defun npm-mode-npm-list ()
    "Run the 'yarn list' command."
    (interactive)
    (el-patch-swap (npm-mode--exec-process "npm list --depth=0")
                   (npm-mode--exec-process "yarn list --depth=0")))
  (defun npm-mode-npm-install ()
    "Run the 'yarn' command."
    (interactive)
    (el-patch-swap (npm-mode--exec-process "npm install")
                   (npm-mode--exec-process "yarn")))
  (defun npm-mode-npm-install-save (dep)
    "Run the 'yarn add' command for DEP."
    (interactive "sEnter package name: ")
    (el-patch-swap (npm-mode--exec-process (format "npm install %s --save" dep))
                   (npm-mode--exec-process (format "yarn add %s" dep))))
  (defun npm-mode--get-project-property (prop)
    "Get the given PROP from the current project file."
    (let* ((project-file (npm-mode--project-file))
           (json-object-type 'hash-table)
           (json-contents (with-temp-buffer
                            (insert-file-contents project-file)
                            (buffer-string)))
           (json-hash (json-read-from-string json-contents))
           (value (gethash prop json-hash))
           (commands (list)))
      (cond ((hash-table-p value)
             (maphash (lambda (key value)
                        (setq commands
                              (append commands
                                      (list (list key (el-patch-swap (format "%s %s" "npm" key)
                                                                     (format "%s %s" "yarn" key)))))))
                      value)
             commands)
            (t value))))
  (defun npm-mode-npm-init ()
    "Run the yarn init command."
    (interactive)
    (el-patch-swap (npm-mode--exec-process "npm init")
                   (npm-mode--exec-process "yarn init")))
  (defun npm-mode-npm-install-save-dev (dep)
    "Run the 'yarn add --dev' command for DEP."
    (interactive "sEnter package name: ")
    (el-patch-swap (npm-mode--exec-process (format "npm install %s --save-dev" dep))
                   (npm-mode--exec-process (format "yarn add --dev %s" dep))))
  (defun npm-mode-npm-uninstall ()
    "Run the 'yarn remove' command."
    (interactive)
    (let ((dep (completing-read "Uninstall dependency: " (npm-mode--get-project-dependencies))))
      (npm-mode--exec-process (el-patch-swap (format "npm uninstall %s" dep)
                                             (format "yarn remove %s" dep)))))
  (defun npm-mode-npm-run (script &optional comint)
    "Run the 'npm run' command on a project script."
    (interactive
     (list (npm-run--read-command)
           (consp current-prefix-arg)))
    (npm-mode--exec-process (el-patch-swap (format "npm run %s" script)
                                           (format "yarn run %s" script)) comint))
  (defun npm-mode--exec-process (cmd &optional comint)
    "Execute a process running CMD."
    (let ((compilation-buffer-name-function
           (lambda (mode)
             (el-patch-swap (format "*npm:%s - %s*"
                                    (npm-mode--get-project-property "name") cmd)
                            (format "*yarn:%s - %s*"
                                    (npm-mode--get-project-property "name") cmd)))))
      (message (concat "Running " cmd))
      (compile cmd comint))))


(use-package jest
  :straight t
  :hook ((rjsx-mode typescript-mode js2-mode) . jest-minor-mode)
  :config
  (push '("^\*jest\*<.*>$" :regexp t   :dedicated t   :position bottom             :noselect t) popwin:special-display-config)
  (evil-define-key 'normal typescript-mode-map ",jj" #'jest-popup)
  (evil-define-key 'normal js2-mode-map ",jj" #'jest-popup))