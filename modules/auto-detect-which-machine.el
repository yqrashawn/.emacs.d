;;; package --- Summary
;;; Commentary:
;;; Code:
(defun yq/macbook ()
  "Load macbook theme and font size."
  (interactive)
  (if (yq/day-p)
      ;; (load-theme 'doom-nord-light)
      (load-theme 'zenburn)
    (load-theme 'zenburn))
  (spacemacs/set-default-font yq/font13))

(defun yq/imac ()
  "Load macbook theme and font size."
  (interactive)
  (if (yq/day-p)
      ;; (load-theme 'doom-nord-light)
      (load-theme 'zenburn)
    (load-theme 'zenburn))
  (spacemacs/set-default-font yq/font15))

(defun yq/home-imac ()
  "Load macbook theme and font size."
  (interactive)
  (if (yq/day-p)
      ;; (load-theme 'doom-nord-light)
      (load-theme 'zenburn)
    (load-theme 'zenburn))
  (spacemacs/set-default-font yq/font18))


(defun yq/day-p ()
  "Return t/nil if it's day or night."
  (interactive)
  (let ((current-hour (nth 2 (decode-time))))
    (and (> current-hour 8) (< current-hour 18))))

(cond ((file-exists-p "~/yq.machine.macbook") (yq/macbook))
      ((file-exists-p "~/yq.machine.home-imac") (yq/home-imac))
      (t (yq/imac)))

(provide 'auto-detect-which-machine)
;;; auto-detect-which-machine.el ends here
