(use-package tide
  :straight t
  :defer t
  :commands (typescript/jump-to-type-def)
  :init
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
  (evil-define-key 'normal typescript-mode-map ",gb" tide-jump-back)
  (evil-define-key 'normal typescript-mode-map ",gt" typescript/jump-to-type-def)
  (evil-define-key 'normal typescript-mode-map ",gu" tide-references)
  (evil-define-key 'normal typescript-mode-map ",hh" tide-documentation-at-point)
  (evil-define-key 'normal typescript-mode-map ",rr" tide-rename-symbol)
  (evil-define-key 'normal typescript-mode-map ",sr" tide-restart-server))
