;;; core-jump.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar spacemacs-default-jump-handlers '()
  "List of jump handlers available in every mode.")

(defvar-local spacemacs-jump-handlers '()
  "List of jump handlers local to this buffer.")

(defmacro spacemacs|define-jump-handlers (mode &rest handlers)
  "Defines jump handlers for the given MODE.
This defines a variable `spacemacs-jump-handlers-MODE' to which
handlers can be added, and a function added to MODE-hook which
sets `spacemacs-jump-handlers' in buffers of that mode."
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (func (intern (format "spacemacs//init-jump-handlers-%S" mode)))
        (handlers-list (intern (format "spacemacs-jump-handlers-%S" mode))))
    `(progn
       (defvar ,handlers-list ',handlers
         ,(format (concat "List of mode-specific jump handlers for %S. "
                          "These take priority over those in "
                          "`spacemacs-default-jump-handlers'.")
                  mode))
       (defun ,func ()
         (setq spacemacs-jump-handlers
               (append ,handlers-list
                       spacemacs-default-jump-handlers)))
       (add-hook ',mode-hook ',func))))

(defun spacemacs/jump-to-definition ()
  "Jump to definition around point using the best tool for this action."
  (interactive)
  (catch 'done
    (let ((old-buffer (current-buffer))
          (old-point (point)))
      (dolist (-handler spacemacs-jump-handlers)
        (let ((handler (if (listp -handler) (car -handler) -handler))
              (async (when (listp -handler)
                       (plist-get (cdr -handler) :async))))
          (ignore-errors
            (call-interactively handler))
          (when (or (eq async t)
                    (and (fboundp async) (funcall async))
                    (not (eq old-point (point)))
                    (not (equal old-buffer (current-buffer))))
            (when better-jumper-local-mode (better-jumper-set-jump))
            (throw 'done t)))))
    (message "No jump handler was able to find this symbol.")))

(defun spacemacs/jump-to-definition-other-window ()
  "Jump to definition around point in other window."
  (interactive)
  (let ((pos (point)))
    ;; since `spacemacs/jump-to-definition' can be asynchronous we cannot use
    ;; `save-excursion' here, so we have to bear with the jumpy behavior.
    (switch-to-buffer-other-window (current-buffer))
    (goto-char pos)
    (spacemacs/jump-to-definition)))

;; Set the `:jump' property manually instead of just using `evil-define-motion'
;; in an `eval-after-load' macro invocation because doing that prevents
;; `describe-function' from correctly finding the source.
;;
;; See discussion on https://github.com/syl20bnr/spacemacs/pull/6771

(with-eval-after-load 'evil
  (evil-set-command-property 'spacemacs/jump-to-definition :jump t))

(provide 'core-jump)
