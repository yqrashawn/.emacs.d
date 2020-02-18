(setq terminal-keybinding-fix-prefix "M-+ M-+ ")
(setq ctrl-keys-to-remap '(?\; ?: ?' ?\" ?. ?> ?, ?< ?/ ?? ?- ?= ?+))

(dolist (char ctrl-keys-to-remap)
  (let ((from (concat terminal-keybinding-fix-prefix "C " (char-to-string char)))
        (to (concat "C-" (char-to-string char))))
    (define-key key-translation-map (kbd from) (kbd to))))

;; (add-hook 'evil-insert-state-entry-hook (lambda () (when (not (display-graphic-p)) (send-string-to-terminal "\033[5 q"))))
;; (add-hook 'evil-normal-state-entry-hook (lambda () (when (not (display-graphic-p)) (send-string-to-terminal "\033[0 q"))))

(use-package term-cursor
  :straight (:host github :repo "h0d/term-cursor.el")
  :defer t
  :init
  (add-hook 'buffer-list-update-hook
            (defl () (if (display-graphic-p)
                         (term-cursor-mode 0)
                       (term-cursor-mode 1)))))

(use-package osx-clipboard
  :straight t
  :diminish osx-clipboard-mode
  :defer t
  :init
  (add-hook
   'buffer-list-update-hook
   (defl ()
     (if (display-graphic-p)
         (osx-clipboard-mode 0)
       (osx-clipboard-mode 1)))))