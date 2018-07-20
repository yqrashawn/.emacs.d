;;; package --- Summary
;;; Commentary:
;;; Code:
(defun yq/macbook ()
  "Load macbook theme and font size."
  (interactive)
  (if (yq/day-p)
      (load-theme 'spacemacs-light)
    (load-theme 'zenburn))
  (spacemacs/set-default-font yq/small-screen-default-font))

(defun yq/imac ()
  "Load macbook theme and font size."
  (interactive)
  (if (yq/day-p)
      (load-theme 'spacemacs-light)
    (load-theme 'zenburn))
  (spacemacs/set-default-font yq/large-screen-default-font))


(defun yq/day-p ()
  "Return t/nil if it's day or night."
  (interactive)
  (let ((current-hour (nth 2 (decode-time))))
    (and (> current-hour 8) (< current-hour 18))))

(if (file-exists-p "~/yq.machine.macbook") (yq/macbook) (yq/imac))

(provide 'auto-detect-which-machine)
;;; auto-detect-which-machine.el ends here

