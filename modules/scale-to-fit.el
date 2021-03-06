;;; scale-to-fit.el --- Scale text to fit window -*- lexical-binding: t -*-

;; Copyright (C) 2019 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/.emacs.d
;; Package-Requires: ((emacs "24.3") face-remap)

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Automatically scale text to fit the current window width.

;;; Code:

(require 'face-remap)

(defconst scale-to-fit--debug nil)

(defcustom scale-to-fit-min-scale -2 "Minimum scale for fitting."
  :type 'number
  :safe #'numberp
  :group 'scale-to-fit)

(defcustom scale-to-fit-max-scale 0 "Maximum scale for fitting."
  :type 'number
  :safe #'numberp
  :group 'scale-to-fit)

(defcustom scale-to-fit-default-width 100 "Default width for firtting"
  :type 'number
  :safe #'numberp
  :group 'scale-to-fit)

(defun scale-to-fit--current-width ()
  "Calculate the effective width of the screen in columns."
  (- (window-body-width)
     (line-number-display-width)))

(defun scale-to-fit--calculate-scale (width)
  "Calculate the appropriate text scale value to fit WIDTH columns."
  (log (/ (scale-to-fit--current-width) (float width))
       text-scale-mode-step))

(defun scale-to-fit (width)
  "Scale text down if window is narrower than WIDTH columns.
If SHRINK-ONLY is non-nil, do not enlarge text beyond scale 0."
  (interactive)
  (let* ((raw-scale (scale-to-fit--calculate-scale width))
         (scale (min (max raw-scale scale-to-fit-min-scale) scale-to-fit-max-scale)))
    (when scale-to-fit--debug
      (message "Scaling text: curr-width: %d, target: %d, raw scale: %f, adjusted scale: %f"
               (scale-to-fit--current-width) width raw-scale scale))
    (unless (or (= scale text-scale-mode-amount))
      (text-scale-set scale))))

;; (defmacro scale-to-fit-setup ()
;;   "Set hooks to call `scale-text-to-fit' when appropriate.

;; WIDTH should evaluate to the target width in columns.
;; MIN and MAX are buffer-local values for `scale-to-fit-min-scale' and `scale-to-fit-max-scale'."
;;   `(let ((fun (lambda (&optional _)
;;                 (text-scale-mode 1)
;;                 (scale-to-fit ,width))))
;;      (when ,min
;;        (setq-local scale-to-fit-min-scale ,min))
;;      (when ,max
;;        (setq-local scale-to-fit-max-scale ,max))
;;      (add-hook 'hack-local-variables-hook fun)
;;      (add-hook 'buffer-list-update-hook fun)
;;      (add-hook 'window-size-change-functions fun)))

(setq scale-to-fit-on nil)

(defun scale-to-fit-toggle ()
  (interactive)
  (if scale-to-fit-on
      (progn
        (setq scale-to-fit-on nil)
        (text-scale-mode 0)
        (scale-to-fit-disable))
    (progn
      (setq scale-to-fit-on t)
      (text-scale-mode 1)
      (scale-to-fit-enable))))

(defun scale-to-fit-fun ()
  (when scale-to-fit-on
    (if (not text-scale-mode)
        (text-scale-mode 1))
    (scale-to-fit scale-to-fit-default-width)))

(defun scale-to-fit-enable ()
  (add-hook 'hack-local-variables-hook 'scale-to-fit-fun)
  (add-hook 'buffer-list-update-hook 'scale-to-fit-fun)
  (add-hook 'window-size-change-functions 'scale-to-fit-fun))

(defun scale-to-fit-disable ()
  (remove-hook 'hack-local-variables-hook 'scale-to-fit-fun)
  (remove-hook 'buffer-list-update-hook 'scale-to-fit-fun)
  (remove-hook 'window-size-change-functions 'scale-to-fit-fun))

(provide 'scale-to-fit)
;;; scale-to-fit.el ends here