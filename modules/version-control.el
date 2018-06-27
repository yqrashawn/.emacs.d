(setq vc-handled-backends '(Git))
(remove-hook 'find-file-hooks 'vc-find-file-hook)
(defun git-get-current-file-relative-path ()
  (replace-regexp-in-string (concat "^" (file-name-as-directory default-directory))
                            ""
                            buffer-file-name))

(defun git-add-current-file ()
  "git add file of current buffer"
  (interactive)
  (let ((filename))
    (when buffer-file-name
      (setq filename (git-get-current-file-relative-path))
      (shell-command (concat "git add " filename))
      (message "DONE! git add %s" filename))))

(defun my-reshape-git-gutter (gutter)
  "Re-shape gutter for `ivy-read'."
  (let* ((linenum-start (aref gutter 3))
         (linenum-end (aref gutter 4))
         (target-line "")
         (target-linenum 1)
         (tmp-line "")
         (max-line-length 0))
    (save-excursion
      (while (<= linenum-start linenum-end)
        (goto-line linenum-start)
        (setq tmp-line (replace-regexp-in-string "^[ \t]*" ""
                                                 (buffer-substring (line-beginning-position)
                                                                   (line-end-position))))
        (when (> (length tmp-line) max-line-length)
          (setq target-linenum linenum-start)
          (setq target-line tmp-line)
          (setq max-line-length (length tmp-line)))

        (setq linenum-start (1+ linenum-start))))
    ;; build (key . linenum-start)
    (cons (format "%s %d: %s"
                  (if (eq 'deleted (aref gutter 1)) "-" "+")
                  target-linenum target-line)
          target-linenum)))

(defun my-goto-git-gutter ()
  (interactive)
  (if git-gutter:diffinfos
      (ivy-read "git-gutters:"
                (mapcar 'my-reshape-git-gutter git-gutter:diffinfos)
                :action (lambda (e)
                          (unless (numberp e) (setq e (cdr e)))
                          (goto-line e)))
    (message "NO git-gutters!")))

(use-package git-commit
  :straight t)
(use-package magit-popup
  :straight t)

(use-package magit
  :straight t
  :config
  (let ((maps (list magit-status-mode-map magit-log-mode-map magit-reflog-mode-map magit-diff-mode-map)))
    (dolist (map maps)
      (evil-define-key 'normal map "j" 'magit-next-line)
      (evil-define-key 'normal map "k" 'magit-previous-line)))
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-mode)
  (add-hook 'git-rebase-mode-hook 'turn-off-evil-snipe-mode)
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
  (add-hook 'git-rebase-mode-hook 'turn-off-evil-snipe-override-mode)
  (spacemacs/set-leader-keys "g" nil)
  (spacemacs/set-leader-keys "gf" 'magit-file-popup)
  (spacemacs/set-leader-keys "gs" 'magit-status))

(use-package evil-magit :straight t)

(use-package with-editor
  :straight t
  :commands (with-editor-mode)
  :hook (git-commit-mode-hook . with-editor-hook)
  :init
  (spacemacs/set-leader-keys "hdK" 'describe-keymap))

(use-package magithub
  :straight t
  :after magit
  :config
  (setq magithub-enabled-by-default nil)
  (setq magithub-features t
        magithub-feature-autoinject t
        magithub-dir "~/Dropbox/sync/magithub"))

(use-package git-gutter
  :straight t
  :diminish git-gutter-mode
  :init
  (global-git-gutter-mode)
  (custom-set-variables '(git-gutter:handled-backends '(git)))
  (global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
  (global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
  (global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
  (global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)
  :config
  (defun my-goto-next-hunk (arg)
    (interactive "p")
    (if (memq major-mode '(diff-mode))
        (diff-hunk-next)
      (forward-line)
      (if (re-search-forward "\\(^<<<<<<<\\|^=======\\|^>>>>>>>\\)" (point-max) t)
          (goto-char (line-beginning-position))
        (forward-line -1)
        (git-gutter:next-hunk arg))))
  (defun my-goto-previous-hunk (arg)
    (interactive "p")
    (if (memq major-mode '(diff-mode))
        (diff-hunk-prev)
      (forward-line -1)
      (if (re-search-backward "\\(^>>>>>>>\\|^=======\\|^<<<<<<<\\)" (point-min) t)
          (goto-char (line-beginning-position))
        (forward-line -1)
        (git-gutter:previous-hunk arg))))
  (define-key evil-normal-state-map "]h" 'git-gutter:next-hunk)
  (define-key evil-normal-state-map "[h" 'git-gutter:previous-hunk))

(use-package git-timemachine
  :straight t
  :commands (git-timemachine)
  :init
  (spacemacs/set-leader-keys
    "gt" 'git-timemachine)
  (evil-define-key 'normal git-timemachine-mode-map
    "n" 'git-timemachine-show-next-revision
    "p" 'git-timemachine-show-previous-revision
    "q" 'git-timemachine-quit
    "W" 'git-timemachine-kill-revision
    "c" 'git-timemachine-show-current-revision)
  (add-hook 'git-timemachine-mode-hook 'yq/fix-evil-state-bug))