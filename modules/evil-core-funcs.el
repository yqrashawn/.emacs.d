(defun spacemacs/system-is-mac ()
  (eq system-type 'darwin))
(defun spacemacs/system-is-linux ()
  (eq system-type 'gnu/linux))
(defun spacemacs/system-is-mswindows ()
  (eq system-type 'windows-nt))

(defvar spacemacs-indent-sensitive-modes
  '(asm-mode
    coffee-mode
    elm-mode
    haml-mode
    haskell-mode
    slim-mode
    makefile-mode
    makefile-bsdmake-mode
    makefile-gmake-mode
    makefile-imake-mode
    python-mode
    yaml-mode)
  "Modes for which auto-indenting is suppressed.")

(defun spacemacs/evil-smart-doc-lookup ()
  "Run documentation lookup command specific to the major mode.
Use command bound to `SPC m h h` if defined, otherwise fall back
to `evil-lookup'"
  (interactive)
  (let ((binding (key-binding (kbd (concat "SPC" " mhh")))))
    (if (commandp binding)
        (call-interactively binding)
      (evil-lookup))))

(defun spacemacs/mplist-get (plist prop)
  "Get the values associated to PROP in PLIST, a modified plist.

A modified plist is one where keys are keywords and values are
all non-keywords elements that follow it.

If there are multiple properties with the same keyword, only the first property
and its values is returned.

Currently this function infloops when the list is circular."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (pop tail))
    ;; pop the found keyword
    (pop tail)
    (while (and (consp tail) (not (keywordp (car tail))))
      (push (pop tail) result))
    (nreverse result)))

(defun spacemacs/mplist-remove (plist prop)
  "Return a copy of a modified PLIST without PROP and its values.

If there are multiple properties with the same keyword, only the first property
and its values are removed."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (push (pop tail) result))
    (when (eq prop (car tail))
      (pop tail)
      (while (and (consp tail) (not (keywordp (car tail))))
        (pop tail)))
    (while (consp tail)
      (push (pop tail) result))
    (nreverse result)))

(defun spacemacs/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window)))
    ;; if no window is found in the windows history, `switch-to-buffer' will
    ;; default to calling `other-buffer'.
    (switch-to-buffer
     (cl-find-if (lambda (buffer)
                   (not (eq buffer current-buffer)))
                 (mapcar #'car (window-prev-buffers window))))))

;; indent on paste
;; from Prelude: https://github.com/bbatsov/prelude
(defun spacemacs/yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) spacemacs-yank-indent-threshold)
      (indent-region beg end nil)))

(defcustom spacemacs-yank-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur."
  :type 'number
  :group 'spacemacs)

(defmacro spacemacs|advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.
The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command
                      (,class ,(intern (format "%S-%s" command advice-name))
                              activate)
                    ,@body))
               commands)))

(spacemacs|advise-commands
 "indent" (yank yank-pop evil-paste-before evil-paste-after) around
 "If current mode is not one of spacemacs-indent-sensitive-modes
 indent yanked text (with universal arg don't indent)."
 (evil-start-undo-step)
 ad-do-it
 (if (and (not (equal '(4) (ad-get-arg 0)))
          (not (member major-mode spacemacs-indent-sensitive-modes))
          (or (derived-mode-p 'prog-mode)
              (member major-mode spacemacs-indent-sensitive-modes)))
     (let ((transient-mark-mode nil)
           (save-undo buffer-undo-list))
       (spacemacs/yank-advised-indent-function (region-beginning)
                                               (region-end))))
 (evil-end-undo-step))

(defun yq/toggle-visual-line-mode ()
  (interactive)
  (when (bound-and-true-p evil-escape-mode)
    (evil-escape-mode -1)
    (setq evil-escape-motion-state-shadowed-func nil)
    (evil-define-minor-mode-key 'motion 'visual-line-mode "j" 'evil-next-visual-line)
    (evil-define-minor-mode-key 'motion 'visual-line-mode "k" 'evil-previous-visual-line)
    (evil-escape-mode))
  (evil-normalize-keymaps))

(defun yq/toggle-mode (mode)
  (message (format "%s" (boundp mode) ))
  (if (bound-and-true-p mode)
      (funcall-interactively mode -1)))
(yq/toggle-mode 'auto-fill-mode)
