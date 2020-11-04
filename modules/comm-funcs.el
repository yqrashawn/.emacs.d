;;; tool-funcs.el --- common funcs                   -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Rashawn Zhang

;; Author: Rashawn Zhang <namy.19@gmail.com>
;; Keywords: comm
(defun yq/scratch-buffer-p ()
  (string= (buffer-name (current-buffer)) "*scratch*"))

(defun yq/js-timestamp-to-str ()
  (interactive)
  (let* ((timestamp (number-at-point))
         (timestr (format-time-string "%F %T" (seconds-to-time (/ timestamp 1000.000)))))
    (message timestr)
    (kill-new timestr)))

;; px->rem
(defvar px->rem-base-px nil)

(defun px->rem-get-base-px ()
  (or (and px->rem-base-px (float px->rem-base-px))
      (progn (setq-local px->rem-base-px (read-number "The base font-size: "))
             (float px->rem-base-px))))

(defun px->rem-num-to-rem (num)
  (concat (string-trim-right (format "%.4f" num) (rx (? ".") (* "0") line-end)) "rem"))

(defun px->rem ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (rx (? (group (* digit) (any "."))) (+ digit) "px") (point-max) t)
      (let ((str (match-string 0))
            (start (match-beginning 0))
            (end (match-end 0)))
        (when (y-or-n-p (format "base font-size is %spx. Change the px here? " (px->rem-get-base-px)))
          (let* ((px (string-to-number (string-remove-suffix "px" str)))
                 (rem (/ px (px->rem-get-base-px))))
            (print (px->rem-num-to-rem rem))
            (replace-region-contents start end (lambda () (px->rem-num-to-rem rem)))))))))

(use-package ts
  :straight t
  :disabled
  :commands (ts-now ts-unix ts-format))

(use-package sx
  :straight t
  :disabled
  :commands (sx-search sx-ask sx-inbox sx-tab-week sx-tab-unanswered-my-tags sx-tab-all-questions)
  :init
  (spacemacs/set-leader-keys "sx" nil)
  (spacemacs/set-leader-keys "sxs" #'sx-search)
  (spacemacs/set-leader-keys "sxi" #'sx-inbox)
  (spacemacs/set-leader-keys "sxw" #'sx-tab-week)
  (spacemacs/set-leader-keys "sxq" #'sx-tab-all-questions)
  (spacemacs/set-leader-keys "sxu" #'sx-tab-unanswered-my-tags)
  (spacemacs/set-leader-keys "sxA" #'sx-ask)
  :config
  (evilified-state-evilify-map sx-question-list-mode-map
    :bindings
    "j" #'sx-question-list-next
    "k" #'sx-question-list-previous
    "n" #'sx-question-list-view-next
    "p" #'sx-question-list-view-previous
    "q" #'quit-window)

  (evilified-state-evilify-map sx-question-mode-map
    :bindings
    "q" #'yq/kill-buffer-and-window))
