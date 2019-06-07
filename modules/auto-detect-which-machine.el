;;; package --- Summary
;;; Commentary:
;;; Code:
(defun yq/macbook ()
  "Load macbook theme and font size."
  (interactive)
  (if (yq/day-p)
      (load-theme yq/light-theme 'no-confirm)
    (load-theme yq/dark-theme 'no-confirm))
  (spacemacs/set-default-font yq/font13))

(defun yq/imac ()
  "Load macbook theme and font size."
  (interactive)
  (if (yq/day-p)
      (load-theme yq/light-theme 'no-confirm)
    (load-theme yq/dark-theme 'no-confirm))
  (spacemacs/set-default-font yq/font15))

(defun yq/home-imac ()
  "Load macbook theme and font size."
  (interactive)
  (if (yq/day-p)
      (load-theme yq/light-theme 'no-confirm)
    (load-theme yq/dark-theme 'no-confirm))
  (spacemacs/set-default-font yq/font15))

(defun yq/day-p ()
  "Return t/nil if it's day or night."
  (interactive)
  (let ((current-hour (nth 2 (decode-time))))
    (and (> current-hour 8) (< current-hour 18))))

(defun yq/toggle-theme ()
  "Toggle between light dark theme"
  (interactive)
  (if (eq spacemacs--cur-theme yq/light-theme)
      (load-theme yq/dark-theme 'no-confirm)
    (load-theme yq/light-theme 'no-confirm)))
(spacemacs/set-leader-keys "tm" 'yq/toggle-theme)

(cond ((file-exists-p "~/yq.machine.macbook") (yq/macbook))
      ((file-exists-p "~/yq.machine.home-imac") (yq/home-imac))
      (t (yq/imac)))

(provide 'auto-detect-which-machine)
;;; auto-detect-which-machine.el ends here
