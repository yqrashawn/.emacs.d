(use-package css-mode
  :straight t
  :defer t
  :init
  (setq css-indent-offset 2)
  ;; Mark `css-indent-offset' as safe-local variable
  (put 'css-indent-offset 'safe-local-variable #'integerp)

  ;; Explicitly run prog-mode hooks since css-mode does not derive from
  ;; prog-mode major-mode in Emacs 24 and below.

  (defun css-expand-statement ()
    "Expand CSS block"
    (interactive)
    (save-excursion
      (end-of-line)
      (search-backward "{")
      (forward-char 1)
      (while (or (eobp) (not (looking-at "}")))
        (let ((beg (point)))
          (newline)
          (search-forward ";")
          (indent-region beg (point))))

      (newline)))

  (defun css-contract-statement ()
    "Contract CSS block"
    (interactive)
    (end-of-line)
    (search-backward "{")
    (while (not (looking-at "}"))
      (join-line -1)))

  (evil-define-key 'normal css-mode-map
    ",zc" 'css-contract-statement
    ",zo" 'css-expand-statement))

(use-package counsel-css
  :straight t
  :commands counsel-css
  :init (evil-define-key 'normal web-mode-map ",l" #'counsel-css))

(use-package less-css-mode
  :straight t
  :mode ("\\.less\\'" . less-css-mode))

(use-package scss-mode
  :straight t
  :mode ("\\.scss\\'" . scss-mode))

(use-package counsel-css
  :straight t
  :commands (counsel-css)
  :init
  (evil-define-key 'normal css-mode-map ",l" 'counsel-css))

(use-package show-css
  :straight t
  :commands (showcss-mode showcss/main)
  :init
  (evil-define-key 'normal web-mode-map
    ",ss" 'showcss-mode
    ",ss" 'showcss/main))