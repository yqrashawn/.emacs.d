(spacemacs|define-jump-handlers typescript-mode)
(defvar typescript-fmt-tool 'tide
  "The name of the tool to be used
for TypeScript source code formatting.
Currently avaliable 'tide (default)
and 'typescript-formatter .")
(defvar typescript-fmt-on-save nil "Run formatter on buffer save.")

(use-package typescript-mode
  :straight t
  :mode ("\\.ts\\'" . typescript-mode)
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
  :init
  (add-hook 'typescript-mode-hook 'eldoc-mode)
  (spacemacs|define-jump-handlers typescript-mode)
  (spacemacs|define-jump-handlers js2-mode)
  (evilified-state-evilify tide-references-mode tide-references-mode-map
    (kbd "C-k") 'tide-find-previous-reference
    (kbd "C-j") 'tide-find-next-reference
    (kbd "C-l") 'tide-goto-reference)
  (add-hook 'typescript-mode-hook 'tide-setup)
  (add-hook 'js2-mode-hook 'tide-setup)
  (add-to-list 'spacemacs-jump-handlers-js2-mode
               '(tide-jump-to-definition :async t))
  (add-to-list 'spacemacs-jump-handlers-typescript-mode
               '(tide-jump-to-definition :async t))
  :config
  (flycheck-add-mode 'typescript-tslint 'typescript-mode)
  (spacemacs/enable-flycheck 'typescript-mode)
  (spacemacs|add-company-backends
    :backends company-tide
    :modes typescript-mode web-mode js2-mode)
  (evil-define-key 'normal typescript-mode-map ",gb" 'tide-jump-back)
  (evil-define-key 'normal typescript-mode-map ",gt" 'typescript/jump-to-type-def)
  (evil-define-key 'normal typescript-mode-map ",gu" 'tide-references)
  (evil-define-key 'normal typescript-mode-map ",hh" 'tide-documentation-at-point)
  (evil-define-key 'normal typescript-mode-map ",rr" 'tide-rename-symbol)
  (evil-define-key 'normal typescript-mode-map ",sr" 'tide-restart-server))
