(defun spacemacs/evil-smart-doc-lookup ()
  "Run documentation lookup command specific to the major mode.
Use command bound to `SPC m h h` if defined, otherwise fall back
to `evil-lookup'"
  (interactive)
  (let ((binding (key-binding (kbd (concat "SPC" " mhh")))))
    (if (commandp binding)
	(call-interactively binding)
      (evil-lookup))))
