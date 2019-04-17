;;; homerow.el --- Selet things with homerow  -*- lexical-binding: t; -*-

;; Copyright © 2019, Rashawn Zhang

;; Author: Rashawn Zhang <hi@yqrashawn.com>
;; Created: 16 April 2019
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;  homerow.el provides minor modes homerow-ivy homerow-company and
;;  homerow-tabbar for Emacs. Enable user to select things with homerow.

;;; Code:

(require 'dash)

(defgroup homerow nil
  "Customization options for homerow"
  :group 'convenience
  :prefix "homerow-")

(defcustom homerow-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?\; ?q ?w ?e ?r
                             ?t ?y ?u ?i ?o ?p ?z ?x ?c ?v ?b ?n ?m
                             ?A ?S ?D ?F ?G ?H ?J ?K ?L ?Q ?W ?E ?R
                             ?T ?Y ?U ?I ?O ?P ?Z ?X ?C ?V ?B ?N ?M)
  "Keys for selecting things."
  :group 'homerow
  :type  '(repeat :tag "Keys" (choice
                               (character :tag "char")
                               (symbol :tag "non-printing key"))))

(defvar homerow--ivy-format-function-backup nil)

;; ivy
(defun homerow--ivy-format-function (cands)
  "Transform CANDS into a string for minibuffer."
  (let ((cands (--map-indexed (format "%s %s" (char-to-string (elt yq-quick-keys it-index)) it) cands)))
    (ivy--format-function-generic
     (lambda (str)
       (ivy--add-face str 'ivy-current-match))
     #'identity
     cands
     "\n")))

(defun homerow-ivy-select-index (&optional key)
  (interactive)
  (let ((key (or key (read-char "key: " t))))
    (ivy-next-line (seq-position homerow-keys key))
    (ivy--exhibit)
    (ivy-alt-done)))

(defun homerow--define-key (key func)
  (define-key ivy-minibuffer-map (kbd (concat "C-x C-6 " (char-to-string key))) func))

(defun homerow-setup-ivy-minibuffer-keymap ()
  "When `homerow-ivy-minibuffer-mode' is on."
  (-map (lambda (key)
          (homerow--define-key
           key
           (lambda () (interactive) (homerow-ivy-select-index key))))
        homerow-keys))

(defun homerow-cleanup-ivy-minibuffer-keymap ()
  "When `homerow-ivy-minibuffer-mode' is off."
  (-map (lambda (key)
          (homerow--define-key key nil))
        homerow-keys))

;;;###autoload
(define-minor-mode homerow-ivy-mode
  ""
  :group 'homerow
  :require 'homerow
  :global t
  (if homerow-ivy-mode
      (progn
        (homerow-cleanup-ivy-minibuffer-keymap))
    (progn
      (setq homerow--ivy-format-function-backup ivy-format-function)
      (setq ivy-format-function 'homerow--ivy-format-function)
      (homerow-setup-ivy-minibuffer-keymap))))

(provide 'homerow)
;;; homerow.el ends here