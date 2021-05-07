(setq terminal-keybinding-fix-prefix "M-+ M-+ ")
(setq ctrl-keys-to-remap '(?\; ?: ?' ?\" ?. ?> ?, ?< ?/ ?? ?- ?= ?+))
(setq command-keys-to-remap '(?k))

(dolist (char ctrl-keys-to-remap)
  (let ((from (concat terminal-keybinding-fix-prefix "C " (char-to-string char)))
        (to (concat "C-" (char-to-string char))))
    (define-key key-translation-map (kbd from) (kbd to))))

(dolist (char command-keys-to-remap)
  (let ((from (concat terminal-keybinding-fix-prefix "D " (char-to-string char)))
        (to (concat "s-" (char-to-string char))))
    (define-key key-translation-map (kbd from) (kbd to))))

;; (add-hook 'evil-insert-state-entry-hook (lambda () (when (not (display-graphic-p)) (send-string-to-terminal "\033[5 q"))))
;; (add-hook 'evil-normal-state-entry-hook (lambda () (when (not (display-graphic-p)) (send-string-to-terminal "\033[0 q"))))

(define-key key-translation-map (kbd (concat terminal-keybinding-fix-prefix "W")) (kbd "C-M-s-7"))

(use-package term-cursor
  :straight (:host github :repo "h0d/term-cursor.el")
  :disabled
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

(use-package emamux
  :straight t
  :defer t
  :commands (emamux:tmux-run-command emamux:check-tmux-running)
  :init
  (when (not (display-graphic-p))
    (defun yq/tmux-command (&rest args)
      (message "yq/tmux-command")
      (emamux:check-tmux-running)
      (apply #'emamux:tmux-run-command nil args))
    (defun yq/tmux-select-pane (dir)
      (if (or (ignore-errors (funcall (intern (concat "windmove-" dir)))) (display-graphic-p)) nil
        (cond ((string= dir "up")
               (yq/tmux-command "select-pane" "-U"))
              ((string= dir "down")
               (yq/tmux-command "select-pane" "-D"))
              ((string= dir "left")
               (yq/tmux-command "select-pane" "-L"))
              ((string= dir "right")
               (yq/tmux-command "select-pane" "-R")))))
    (defun yq/split-window-right-tmux (arg)
      (interactive "P")
      (if arg
          (emamux:split-window-horizontally)
        (yq/split-window-right)))

    (defun yq/split-window-below-tmux (arg)
      (interactive "P")
      (if arg
          (emamux:split-window)
        (yq/split-window-below)))

    (defun yq/toggle-maximize-buffer-tmux (arg)
      (interactive "P")
      (if arg
          (yq/tmux-command "resize-pane" "-Z")
        (spacemacs/toggle-maximize-buffer)))
    (global-set-key [remap windmove-up] (defl () (yq/tmux-select-pane "up")))
    (global-set-key [remap windmove-down] (defl () (yq/tmux-select-pane "down")))
    (global-set-key [remap windmove-left] (defl () (yq/tmux-select-pane "left")))
    (global-set-key [remap windmove-right] (defl () (yq/tmux-select-pane "right")))
    (global-set-key [remap yq/split-window-right] 'yq/split-window-right-tmux)
    (global-set-key [remap yq/split-window-below] 'yq/split-window-below-tmux)
    (global-set-key [remap spacemacs/toggle-maximize-buffer] 'yq/toggle-maximize-buffer-tmux)
    (global-set-key (kbd "C-'") #'emamux:split-window)))

(with-eval-after-load 'org
  (evil-define-key 'normal org-mode-map (kbd "TAB") #'org-cycle))