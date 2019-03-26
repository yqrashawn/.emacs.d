;;; tool-funcs.el --- common funcs                   -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Rashawn Zhang

;; Author: Rashawn Zhang <namy.19@gmail.com>
;; Keywords: comm
(defun yq/scratch-buffer-p ()
  (string= (buffer-name (current-buffer)) "*scratch*"))

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
