;;; tool-funcs.el --- common funcs                   -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Rashawn Zhang

;; Author: Rashawn Zhang <namy.19@gmail.com>
;; Keywords: comm
(defun yq/scratch-buffer-p ()
  (string= (buffer-name (current-buffer)) "*scratch*"))