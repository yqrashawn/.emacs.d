(use-package iedit :straight t)

(use-package expand-region
  :straight t
  :config
  (define-key evil-normal-state-map "sv" 'er/expand-region)
  (setq expand-region-contract-fast-key "V")
  expand-region-reset-fast-key "r")

(use-package evil-iedit-state :straight t
  :config
  (define-key evil-iedit-state-map "V" nil)
  (define-key evil-iedit-state-map "m" 'iedit-show/hide-unmatched-lines))

(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(
        ;; Try to expand word before point according to all abbrev tables.
        try-expand-all-abbrevs
        ;; Try to expand word "dynamically", searching the current buffer.
        try-expand-dabbrev
        ;; Try to expand word "dynamically", searching all other buffers.
        try-expand-dabbrev-all-buffers
        ;; Try to expand word "dynamically", searching the kill ring.
        try-expand-dabbrev-from-kill
        ;; Try to complete text as a file name, as many characters as unique.
        try-complete-file-name-partially
        ;; Try to complete text as a file name.
        try-complete-file-name
        ;; Try to complete the current line to an entire line in the buffer.
        try-expand-list
        ;; Try to complete the current line to an entire line in the buffer.
        try-expand-line
        ;; Try to complete as an Emacs Lisp symbol, as many characters as
        ;; unique.
        try-complete-lisp-symbol-partially
        ;; Try to complete word as an Emacs Lisp symbol.
        try-complete-lisp-symbol))
(define-key evil-insert-state-map (kbd "C-l") 'hippie-expand)

(use-package evil-mc
  :straight t
  :diminish evil-mc-mode
  :commands (evil-mc-make-cursor-move-next-line
             evil-mc-make-cursor-move-prev-line
             evil-mc-mode
             evil-mc-undo-all-cursors
             global-evil-mc-mode)
  :init
  (add-hook 'after-init-hook #'global-evil-mc-mode)
  (setq evil-mc-one-cursor-show-mode-line-text nil)
  (advice-add 'keyboard-quit :before #'evil-mc-undo-all-cursors)
  (define-key evil-normal-state-map (kbd "M-j") 'evil-mc-make-cursor-move-next-line)
  (define-key evil-normal-state-map (kbd "M-k") 'evil-mc-make-cursor-move-prev-line)
  (define-key evil-normal-state-map (kbd "<C-return>") 'evil-mc-make-all-cursors)
  :config
  (add-to-list 'evil-mc-known-commands '(mwim-beginning-of-code-or-line (:default . evil-first-non-blank)))
  (add-to-list 'evil-mc-known-commands '(mwim-end-of-code-or-line (:default . evil-end-of-line))))


;; (use-package aggressive-indent
;;   :straight t
;;   :init
;;   (yq/add-toggle aggressive-indent :mode aggressive-indent-mode)
;;   (spacemacs/set-leader-keys "tI" 'yq/toggle-aggressive-indent)
;;   (add-hook 'prog-mode-hook 'aggressive-indent-mode)
;;   (add-hook 'rjsx-mode-hook 'yq/toggle-aggressive-indent-off)
;;   (add-hook 'typescript-mode-hook 'yq/toggle-aggressive-indent-off)
;;   :commands (aggressive-indent-mode global-aggressive-indent-mode)
;;   :diminish aggressive-indent-mode)
