(add-hook 'comint-mode-hook 'yq/toggle-hl-line-off)
(defun spacemacs//init-eshell ()
  (if (string-match ".*sudo.*@" (buffer-name))
      (company-mode -1))
  (evil-emacs-state)
  (setq pcomplete-cycle-completions nil)
  (defun eshell/clear ()
    (let ((inhibit-read-only t))
      (erase-buffer)))
  (defun spacemacs/eshell-clear-keystroke ()
    "Allow for keystrokes to invoke eshell/clear"
    (interactive)
    (eshell/clear)
    (eshell-send-input))
  (define-key eshell-mode-map (kbd "C-l") 'spacemacs/eshell-clear-keystroke)
  (define-key eshell-mode-map (kbd "C-d") 'eshell-delchar-or-maybe-eof))
(add-hook 'eshell-mode-hook 'spacemacs//init-eshell)

(defvar spacemacs-repl-list '()
  "List of all registered REPLs.")
(setq shell-default-shell 'multi-term)
(defvar shell-default-shell (if (eq window-system 'w32)
                                'eshell
                              'ansi-term)
  "Default shell to use in Spacemacs. Possible values are `eshell', `shell',
`term' and `ansi-term'.")

(defvar shell-default-position 'top
  "Position of the shell. Possible values are `top', `bottom', `full',
`left' and `right'.")

(defvar shell-default-height 30
  "Height in percents for the shell window.")

(defvar shell-default-term-shell shell-file-name
  "Default shell to use in `term' and `ansi-term' shells.")

(defvar shell-default-full-span t
  "If non-nil, the `shell' buffer spans full width of a frame.")

(defun spacemacs/default-pop-shell ()
  "Open the default shell in a popup."
  (interactive)
  (let ((shell (if (eq 'multi-term shell-default-shell)
                   'multi-term
                 shell-default-shell)))
    (call-interactively (intern (format "spacemacs/shell-pop-%S" shell)))))

(defun spacemacs/projectile-shell-pop ()
  "Open a term buffer at projectile project root."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (call-interactively 'spacemacs/default-pop-shell)))

(defun spacemacs/register-repl (feature repl-func &optional tag)
  "Register REPL-FUNC to the global list of REPLs SPACEMACS-REPL-LIST.
FEATURE will be loaded before running the REPL, in case it is not already
loaded. If TAG is non-nil, it will be used as the string to show in the helm
buffer."
  (push `(,(or tag (symbol-name repl-func))
          . (,feature . ,repl-func))
        spacemacs-repl-list))

(defmacro make-shell-pop-command (func &optional shell)
  "Create a function to open a shell via the function FUNC.
SHELL is the SHELL function to use (i.e. when FUNC represents a terminal)."
  (let* ((name (symbol-name func)))
    `(defun ,(intern (concat "spacemacs/shell-pop-" name)) (index)
       ,(format (concat "Toggle a popup window with `%S'.\n"
                        "Multiple shells can be opened with a numerical prefix "
                        "argument. Using the universal prefix argument will "
                        "open the shell in the current buffer instead of a "
                        "popup buffer.") func)
       (interactive "P")
       (unless (featurep 'shell-pop)
         (require 'shell-pop))
       (if (equal '(4) index)
           ;; no popup
           (,func ,shell)
         (shell-pop--set-shell-type
          'shell-pop-shell-type
          (backquote (,name
                      ,(concat "*" name "*")
                      (lambda nil (,func)))))
         (shell-pop index)))))

(defun ansi-term-handle-close ()
  "Close current term buffer when `exit' from term buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)"
                                                change)
                              (kill-buffer (process-buffer proc))
                              (when (> (count-windows) 1)
                                (delete-window)))))))

(spacemacs/register-repl 'shell 'shell)

(defun shell-comint-input-sender-hook ()
  "Check certain shell commands.
 Executes the appropriate behavior for certain commands."
  (setq comint-input-sender
        (lambda (proc command)
          (cond
           ;; Check for clear command and execute it.
           ((string-match "^[ \t]*clear[ \t]*$" command)
            (comint-send-string proc "\n")
            (erase-buffer))
           ;; Check for man command and execute it.
           ((string-match "^[ \t]*man[ \t]*" command)
            (comint-send-string proc "\n")
            (setq command (replace-regexp-in-string
                           "^[ \t]*man[ \t]*" "" command))
            (setq command (replace-regexp-in-string
                           "[ \t]+$" "" command))
            (funcall 'man command))
           ;; Send other commands to the default handler.
           (t (comint-simple-send proc command))))))
(add-hook 'shell-mode-hook 'shell-comint-input-sender-hook)
(add-hook 'shell-mode-hook 'yq/toggle-hl-line-off)

(use-package shell-pop
  :straight t
  :defer t
  :init
  (progn
    (setq shell-pop-window-position shell-default-position
          shell-pop-window-size     shell-default-height
          shell-pop-term-shell      shell-default-term-shell
          shell-pop-full-span       shell-default-full-span)
    (make-shell-pop-command eshell)
    ;; (make-shell-pop-command shell)
    ;; (make-shell-pop-command term shell-pop-term-shell)
    (make-shell-pop-command multi-term)
    ;; (make-shell-pop-command ansi-term shell-pop-term-shell)

    (add-hook 'term-mode-hook 'ansi-term-handle-close)
    (add-hook 'term-mode-hook (lambda () (linum-mode -1)))

    (spacemacs/set-leader-keys
      "'"   'spacemacs/default-pop-shell
      "ase" 'spacemacs/shell-pop-eshell)))

;; (spacemacs/register-repl 'term 'term)
;; (spacemacs/register-repl 'term 'ansi-term)

(use-package multi-term
  :straight t
  :commands (multi-term)
  :init
  (spacemacs/register-repl 'multi-term 'multi-term)
  (setq shell-pop-shell-type '("multi-term" "*multi-term" 'multi-term))
  (setq multi-term-program "/usr/local/bin/zsh")
  :config
  (add-to-list 'term-bind-key-alist '("<tab>" . term-send-tab))
  ;; multi-term commands to create terminals and move through them.
  (spacemacs/set-leader-keys "p'" 'spacemacs/projectile-shell-pop)
  (define-key term-mode-map (kbd "s-[") 'multi-term-prev)
  (define-key term-mode-map (kbd "s-<return>") 'multi-term)
  (define-key term-mode-map (kbd "s-]" ) 'multi-term-next)
  (evil-define-key 'insert term-mode-mp
    (kbd "s-<return>") 'multi-term
    (kbd "s-[") 'multi-term-prev
    (kbd "s-]" ) 'multi-term-next)
  (evil-define-key 'normal term-mode-map
    ",c" 'multi-term
    ",p" 'multi-term-prev
    ",n" 'multi-term-next))


(defun term-send-tab ()
  "Send tab in term mode."
  (interactive)
  (term-send-raw-string "\t"))

;; hack to fix pasting issue, the paste transient-state won't
;; work in term
(evil-define-key 'normal term-raw-map "p" 'term-paste)
(evil-define-key 'insert term-raw-map (kbd "C-c C-d") 'term-send-eof)
(evil-define-key 'insert term-raw-map (kbd "C-c C-z") 'term-stop-subjob)
(evil-define-key 'insert term-raw-map (kbd "<tab>") 'term-send-tab)

(evil-define-key 'insert term-raw-map
  (kbd "C-p") 'term-send-up
  (kbd "C-n") 'term-send-down)
(evil-define-key 'normal term-raw-map
  (kbd "C-k") 'term-send-up
  (kbd "C-j") 'term-send-down)

(add-hook 'term-mode-hook 'yq/toggle-hl-line-off)

(use-package xterm-color
  :straight t
  :init
  ;; Comint and Shell
  (defun spacemacs/init-eshell-xterm-color ()
    "Initialize xterm coloring for eshell"
    (setq-local xterm-color-preserve-properties t)
    (make-local-variable 'eshell-preoutput-filter-functions)
    (add-hook 'eshell-preoutput-filter-functions 'xterm-color-filter)
    (setq-local eshell-output-filter-functions
                (remove 'eshell-handle-ansi-color
                        eshell-output-filter-functions)))
  (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (add-hook 'eshell-mode-hook 'spacemacs/init-eshell-xterm-color))

(evil-set-initial-state 'term-mode 'emacs)
(global-set-key (kbd "C-'") 'spacemacs/default-pop-shell)