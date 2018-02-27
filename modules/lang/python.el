(spacemacs|define-jump-handlers python-mode)
(spacemacs|define-jump-handlers cython-mode anaconda-mode-goto)

(use-package anaconda-mode
  :straight t
  :diminish anaconda-mode
  :defer t
  :init
  (setq anaconda-mode-installation-directory
        (concat spacemacs-cache-directory "anaconda-mode"))
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-to-list 'spacemacs-jump-handlers-python-mode
               '(anaconda-mode-find-definitions :async t))
  :config
  (evil-define-key 'normal 'python-mode
    ",hh" 'anaconda-mode-show-doc
    ",ga" 'anaconda-mode-find-assignments
    ",gb" 'anaconda-mode-go-back
    ",gu" 'anaconda-mode-find-references)

  (evilified-state-evilify-map anaconda-view-mode-map
    :mode anaconda-view-mode
    :bindings
    (kbd "q") 'quit-window
    (kbd "C-j") 'next-error-no-select
    (kbd "C-k") 'previous-error-no-select
    (kbd "RET") 'spacemacs/anaconda-view-forward-and-push)

  (defadvice anaconda-mode-goto (before python/anaconda-mode-goto activate)
    (evil--jumps-push))

  ;; eldoc
  (defun spacemacs//init-eldoc-python-mode ()
    "Initialize elddoc for python buffers"
    (eldoc-mode)
    (anaconda-eldoc-mode))
  (add-hook 'python-mode-hook 'spacemacs//init-eldoc-python-mode)
  (spacemacs/enable-flycheck 'python-mode))
(use-package company-anaconda
  :straight t
  :after company
  :init
  (spacemacs|add-company-backends
    :backends company-anaconda
    :modes python-mode))

(use-package importmagic
  :straight t
  :after anaconda-mode
  :init
  (add-hook 'python-mode-hook 'importmagic-mode)
  (evil-define-key 'normal 'python-mode
    ",rf" 'importmagic-fix-symbol-at-point))

(use-package live-py-mode
  :straight t
  :commands live-py-mode
  :init
  (evil-define-key 'normal 'python-mode
    ",l" 'live-py-mode))