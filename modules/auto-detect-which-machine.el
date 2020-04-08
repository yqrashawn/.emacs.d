;;; package --- Summary
;;; Commentary:
;;; Code:
(setq yq-dark-theme t)
(defun yq/macbook ()
  "Load macbook theme and font size."
  (interactive)
  (spacemacs/set-default-font (yq/font 11))
  (if (yq/day-p)
      (load-theme yq/light-theme 'no-confirm)
    (load-theme yq/dark-theme 'no-confirm)))

(defun yq/imac ()
  "Load macbook theme and font size."
  (interactive)
  (if (yq/day-p)
      (load-theme yq/light-theme 'no-confirm)
    (load-theme yq/dark-theme 'no-confirm))
  (spacemacs/set-default-font (yq/font 18)))

(defun yq/day-p ()
  "Return t/nil if it's day or night."
  (interactive)
  (if yq-dark-theme nil
      (let ((current-hour (nth 2 (decode-time))))
        (and (> current-hour 8) (< current-hour 18)))))

(defun yq/toggle-theme ()
  "Toggle between light dark theme"
  (interactive)
  (if (eq spacemacs--cur-theme yq/light-theme)
      (load-theme yq/dark-theme 'no-confirm)
    (load-theme yq/light-theme 'no-confirm)))
(spacemacs/set-leader-keys "tm" 'yq/toggle-theme)

(defun yq/imac-p () (file-exists-p "~/yq.machine.home-imac"))
(defun yq/macbook-p () (file-exists-p "~/yq.machine.macbook"))

(cond ((yq/macbook-p) (yq/macbook))
      ((yq/imac-p) (yq/imac))
      (t (yq/imac)))

(load-theme yq/dark-theme)

(provide 'auto-detect-which-machine)
;;; auto-detect-which-machine.el ends here
