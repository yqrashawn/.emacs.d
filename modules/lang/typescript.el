;;; typescript.el ---  typescript packages -*- lexical-binding: t; -*-

(spacemacs|define-jump-handlers typescript-mode)
(defvar typescript-fmt-tool 'tide
  "The name of the tool to be used
for TypeScript source code formatting.
Currently avaliable 'tide (default)
and 'typescript-formatter .")
(defvar typescript-fmt-on-save nil "Run formatter on buffer save.")

(defun enable-rjsx-feature-in-ts ()
  (evil-define-key 'insert typescript-mode-map "<" 'rjsx-electric-lt)
  (evil-define-key 'insert typescript-mode-map ">" 'rjsx-electric-gt)
  (evil-define-key 'insert typescript-mode-map (kbd "C-d") 'rjsx-delete-creates-full-tag))

(use-package typescript-mode
  :straight t
  :mode (("\\.ts\\'" . typescript-mode) ("\\.tsx\\'" . typescript-mode))
  :init
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode-hook 'enable-rjsx-feature-in-ts)
  ;;; typescript-tsx-mdoe
  ;; (progn (define-derived-mode typescript-tsx-mode web-mode "TypeScript-tsx")
  ;;        (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))

  ;;        ;; (add-hook 'typescript-tsx-mode-hook #'emmet-mode)

  ;;        (with-eval-after-load 'flycheck
  ;;          (flycheck-add-mode 'typescript-tslint 'typescript-tsx-mode)
  ;;          (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)))
  :config
  (defun spacemacs/typescript-format ()
    "Call formatting tool specified in `typescript-fmt-tool'."
    (interactive)
    (cond
     ((eq typescript-fmt-tool 'typescript-formatter)
      (call-interactively 'spacemacs/typescript-tsfmt-format-buffer))
     ((eq typescript-fmt-tool 'tide)
      (call-interactively 'tide-format))
     (t (error (concat "%s isn't valid typescript-fmt-tool value."
                       " It should be 'tide or 'typescript-formatter."
                       (symbol-name typescript-fmt-tool))))))
  (when typescript-fmt-on-save
    (add-hook 'typescript-mode-hook 'spacemacs/typescript-fmt-before-save-hook))
  (evil-define-key 'normal typescript-mode "=" 'spacemacs/typescript-format))

(use-package tide
  :straight t
  :defer t
  :diminish tide-mode
  :commands (typescript/jump-to-type-def)
  :custom
  (tide-completion-detailed t)
  (tide-always-show-documentation t)
  (tide-server-max-response-length 524288)
  (tide-format-options '(:indentSize 2 :tabSize 2 :insertSpaceAfterSemicolonInForStatements t))
  :init
  (defun +javascript-cleanup-tide-processes-h ()
    "Clean up dangling tsserver processes if there are no more buffers with
`tide-mode' active that belong to that server's project."
    (when tide-mode
      (unless (cl-loop with project-name = (tide-project-name)
                       for buf in (delq (current-buffer) (buffer-list))
                       if (and (buffer-local-value 'tide-mode buf)
                               (with-current-buffer buf
                                 (string= (tide-project-name) project-name)))
                       return buf)
        (kill-process (tide-current-server)))))

  (add-hook 'tide-mode-hook (defl () (add-hook 'kill-buffer-hook '+javascript-cleanup-tide-processes-h nil t)))
  (add-hook 'typescript-mode-hook 'tide-hl-identifier-mode)
  (spacemacs|define-jump-handlers typescript-mode)
  ;; (spacemacs|define-jump-handlers js2-mode)
  (advice-add 'tide-setup :before-until 'yq/scratch-buffer-p)
  (evilified-state-evilify tide-references-mode tide-references-mode-map
    (kbd "C-k") 'tide-find-previous-reference
    (kbd "C-j") 'tide-find-next-reference
    (kbd "C-l") 'tide-goto-reference)
  (add-hook 'typescript-mode-hook 'tide-setup)
  ;; (add-hook 'js2-mode-hook 'tide-setup)
  ;; (add-to-list 'spacemacs-jump-handlers-js2-mode
  ;;              '(tide-jump-to-definition :async t))
  (add-to-list 'spacemacs-jump-handlers-typescript-mode
               '(tide-jump-to-definition :async t))
  ;; (flycheck-add-mode 'typescript-tslint 'typescript-mode)
  (spacemacs/enable-flycheck 'typescript-mode)
  (spacemacs|add-company-backends
    :backends (company-capf company-tide)
    :modes typescript-mode ;; web-mode js2-mode
    :after-hook t)
  :config
  (evil-define-key 'normal typescript-mode-map ",gb" 'tide-jump-back)
  (evil-define-key 'normal typescript-mode-map ",gt" 'typescript/jump-to-type-def)
  (evil-define-key 'normal typescript-mode-map ",gu" 'tide-references)
  (evil-define-key 'normal typescript-mode-map ",hh" 'tide-documentation-at-point)
  (evil-define-key 'normal typescript-mode-map ",rr" 'tide-rename-symbol)
  (evil-define-key 'normal typescript-mode-map ",sr" 'tide-restart-server))
