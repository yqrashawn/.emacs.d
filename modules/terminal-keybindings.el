(setq terminal-keybinding-fix-prefix "M-+ M-+ C")
(setq ctrl-keys-to-remap '(?\; ?: ?' ?\" ?. ?> ?, ?< ?/ ?? ?- ?= ?+))

(dolist (char ctrl-keys-to-remap)
  (let ((from (concat terminal-keybinding-fix-prefix (char-to-string char)))
        (to (concat "C-" (char-to-string char))))
    (define-key key-translation-map (kbd from) (kbd to))))