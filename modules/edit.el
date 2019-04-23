(use-package iedit
  :straight t
  :preface
  (defun +iedit-auto-buffering-setup ()
    (setq-local iedit-auto-buffering t))
  :hook
  ((clojure-mode emacs-lisp-mode) . +iedit-auto-buffering-setup))

(use-package expand-region
  :straight t
  :config
  (define-key yq-s-map "v" 'er/expand-region)
  (setq expand-region-contract-fast-key "V"
        expand-region-reset-fast-key "r"))

(use-package evil-iedit-state
  :straight t
  :custom
  (evil-multiedit-store-in-search-history t)
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