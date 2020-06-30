;;; magit-cz.el --- magit commitizen support  -*- lexical-binding: t; -*-

;; Copyright © 2020, yqrashawn, all rights reserved.

;; Author: yqrashawn <namy.19@gmail.com>
;; Created: 28 June 2020
;; Keywords: magit vc

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

;;

;;; Code:
(require 'transient)

(defgroup magit-cz nil
  "Commitizen support for magit")

(defvar magit-cz--breaking-change-on nil)

(defcustom magit-cz-types
  '((?f "fix" "A bug fix" "Bug Fixes")
    (?d "docs" "Documentation only changes" "Documentation")
    (?s "style" "Changes that do not affect the meaning of the code (white-space formatting missing semi-colons etc)" "Styles")
    (?r "refactor" "A code change that neither fixes a bug nor adds a feature" "Code Refactoring")
    (?p "perf" "A code change that improves performance" "Performance Improvements")
    (?t "test" "Adding missing tests or correcting existing tests" "Tests")
    (?b "build" "Changes that affect the build system or external dependencies (example scopes gulp broccoli npm)" "Builds")
    (?C "ci" "Changes to our CI configuration files and scripts (example scopes Travis Circle BrowserStack SauceLabs)" "Continuous Integrations")
    (?c "chore" "Other changes that don't modify src or test files" "Chores")
    (?r "revert" "Reverts a previous commit" "Reverts")
    (?q "quit" "Do nothing" "Quit")
    (? " quit" "Do nothing" "Quit"))
  "Commit type from `conventional-commit-types' npm package"
  :group 'magit-cz)

(defcustom magit-cz-types-prompt-template "[%s]%s - %s"
  "Template for types prompt")

(defvar magit-cz--flag-in-cz nil
  "Indicator of commitizen prompt for functions used in `magit-process-prompt-functions'")
(defvar magit-cz--flag-type-finished nil
  "Indicator of commitizen prompt for functions used in `magit-process-prompt-functions'")
(defvar magit-cz--flag-scope-finished nil
  "Indicator of commitizen prompt for functions used in `magit-process-prompt-functions'")
(defvar magit-cz--flag-short-msg-finished nil
  "Indicator of commitizen prompt for functions used in `magit-process-prompt-functions'")
(defvar magit-cz--flag-long-msg-finished nil
  "Indicator of commitizen prompt for functions used in `magit-process-prompt-functions'")
(defvar magit-cz--flag-breaking-change-p-finished nil
  "Indicator of commitizen prompt for functions used in `magit-process-prompt-functions'")
(defvar magit-cz--flag-breaking-change-body-finished nil
  "Indicator of commitizen prompt for functions used in `magit-process-prompt-functions'")
(defvar magit-cz--flag-breaking-change-des-finished nil
  "Indicator of commitizen prompt for functions used in `magit-process-prompt-functions'")
(defvar magit-cz--flag-issue-p-finished nil
  "Indicator of commitizen prompt for functions used in `magit-process-prompt-functions'")
(defvar magit-cz--flag-issue-num-finished nil
  "Indicator of commitizen prompt for functions used in `magit-process-prompt-functions'")

(defvar magit-cz--breaking-change-body nil "")
(defvar magit-cz--flag-issue-num nil "")

(defun magit-cz--types-message ()
  (propertize
   (concat
    (string-join
     (cons "\nChoose commit type:"
           (mapcar
            (lambda (p)
              (format
               magit-cz-types-prompt-template
               (char-to-string (car p))
               (seq-drop (nth 1 p) 1)
               (nth 2 p)))
            magit-cz-types))
     "\n") "\n")
   'face
   'minibuffer-prompt))

(defun magit-cz-functions-detect-cz (proc s)
  "Function to detect if there's cz prompt"
  (message s)
  (make-local-variable 'magit-cz--flag-in-cz)
  (when (or (string-match-p "cz-cli" s) (string-match-p "cz-conventional-changelog" s))
    (setq magit-cz--flag-in-cz t)
    t))

(defun magit-cz-functions-cz-types (proc s)
  "Function to handle cz types prompt"
  (message s)
  (when (and magit-cz--flag-in-cz (string-match-p "move up and down to reveal" s))
    (let* ((type-index (cl-position (assoc (read-char-choice (magit-cz--types-message) (mapcar 'car magit-cz-types)) magit-cz-types) magit-cz-types))
           (res (concat (string-join (make-list type-index "")) "")))
      (process-send-string proc res)
      (make-local-variable 'magit-cz--flag-type-finished)
      (setq magit-cz--flag-type-finished t))
    t))

(defun magit-cz-functions-cz-scope (proc s)
  "Function to handle cz scope prompt"
  (message s)
  (when (and magit-cz--flag-in-cz (string-match-p "scope" s))
    (message "in scope -------------------------------")
    (let* ((scope (read-string "Commit scope: "))
           (res (concat scope "")))
      (process-send-string proc res)
      (make-local-variable 'magit-cz--flag-scope-finished)
      (setq magit-cz--flag-scope-finished t))
    t))

(defun magit-cz-functions-cz-short-msg (proc s)
  (message s)
  "Function to handle cz scope prompt"
  (when (and magit-cz--flag-in-cz (string-match-p "short.*description" s))
    (process-send-string proc "___MAGIT_CZ_DES__")
    (make-local-variable 'magit-cz--flag-short-msg-finished)
    (setq magit-cz--flag-short-msg-finished t)
    t))

(defun magit-cz-functions-cz-long-msg (proc s)
  "Function to handle cz scope prompt"
  (message s)
  (when (and magit-cz--flag-in-cz (string-match-p "long.*description" s))
    (process-send-string proc "")
    (make-local-variable 'magit-cz--flag-long-msg-finished)
    (setq magit-cz--flag-long-msg-finished t)
    t))

(defun magit-cz-functions-cz-breaking-change-p (proc s)
  "Function to handle cz breaking changes prompt"
  (message s)
  (when (and magit-cz--flag-in-cz (string-match-p "any breaking changes" s))
    (let ((bc-p (y-or-n-p "Is this a breaking change?"))
          (res (if bc-p "y" "n")))
      (process-send-string proc res)
      (make-local-variable 'magit-cz--flag-breaking-change-p-finished)
      (setq magit-cz--flag-breaking-change-p-finished t))
    t))

(defun magit-cz-functions-cz-breaking-change-body (proc s)
  "Function to handle cz breaking changes prompt"
  (message s)
  (when (and magit-cz--flag-in-cz (string-match-p "BREAKING CHANGE commit.*body" s))
    (process-send-string proc "")
    (make-local-variable 'magit-cz--flag-breaking-change-body-finished)
    (setq magit-cz--flag-breaking-change-body-finished t)
    t))

(defun magit-cz-functions-cz-breaking-change-des (proc s)
  "Function to handle cz breaking changes prompt"
  (message s)
  (when (and magit-cz--flag-in-cz (string-match-p "Describe.*breaking changes" s))
    (process-send-string proc "")
    (make-local-variable 'magit-cz--flag-breaking-change-des-finished)
    (setq magit-cz--flag-breaking-change-des-finished t)
    t))

(defun magit-cz-functions-cz-issue-p (proc s)
  "Function to handle cz breaking changes prompt"
  (message s)
  (when (and magit-cz--flag-in-cz (string-match-p "open issue" s))
    (let ((issue-num (read-string "Issue Ref Number: "))
          (issue-p (not (eq (length issue-num) 0)))
          (res (if issue-p "y" "n")))
      (process-send-string proc res)
      (make-local-variable 'magit-cz--flag-issue-p-finished)
      (setq magit-cz--flag-issue-p-finished t)
      (make-local-variable 'magit-cz--flag-issue-num)
      (setq magit-cz--issue-num (and issue-p issue-num)))
    t))

(defun magit-cz-functions-cz-issue-num (proc s)
  "Function to handle cz breaking changes prompt"
  (message s)
  (when (and magit-cz--flag-in-cz (string-match-p "issue references" s))
    (process-send-string proc (conat "resolve #" magit-cz--flag-issue-num ""))
    (make-local-variable 'magit-cz--flag-issue-num-finished)
    (setq magit-cz--flag-issue-num-finished t)
    t))

(defun magit-cz--read-commit-type (prompt initial-input history)
  (car (alist-get (read-char-choice (magit-cz--types-message) (mapcar 'car magit-cz-types)) magit-cz-types)))

;; (defun magit-cz-git-commit-setup-function ()
;;   (let* ((type (car (alist-get (read-char-choice (magit-cz--types-message) (mapcar 'car magit-cz-types)) magit-cz-types)))
;;          (type? (not (or (string= type " quit") (string= type "quit")))))
;;     (when type?
;;       (let* ((scope (read-string "Commit scope: "))
;;              (scope? (not (eq (length scope) 0)))
;;              (bc-body (read-string "Breaking Change Body: "))
;;              (bc? (not (string= bc-body "")))
;;              (msg (if scope? (format "%s(%s): " type scope) (concat type ": ")))
;;              (msg (if bc? (format "%s\n\nBREAKING CHANGE: %s\n" msg bc-body) msg)))
;;         (insert msg)))))

(defun magit-cz-git-commit-setup-function ()
  (let* ((type (magit-get "commit.czType")))
    (when type
      (let* ((scope (magit-get "commit.czScope"))
             (msg (if scope (format "%s(%s): " type scope) (concat type ": ")))
             (msg (if magit-cz--breaking-change-on (format "%s\n\nBREAKING CHANGE: \n" msg) msg)))
        (insert msg))))
  (magit-set "commit.czType" nil)
  (magit-set "commit.czScope" nil)
  (magit-set "commit.czBreakingChange" nil)
  (setq magit-cz--breaking-change-on nil))

(add-hook 'git-commit-setup-hook 'magit-cz-git-commit-setup-function)

(defun magit-cz-functions-detect-and-cancel-cz (proc s)
    (when (or (string-match-p "cz-cli" s) (string-match-p "cz-conventional-changelog" s))
      (process-send-string proc "")
      t))
(add-to-list 'magit-process-prompt-functions 'magit-cz-functions-detect-and-cancel-cz)

(defun magit-cz--breaking-change-toggle (&optional args)
  (interactive)
  (magit-set "commit.czBreakingChange" "true")
  (setq magit-cz--breaking-change-on t)
  (magit-commit-create args))

(transient-define-infix magit-commit:--cz-type ()
  :description "Commit type"
  :class 'magit--git-variable
  :reader 'magit-cz--read-commit-type
  :variable "commit.czType")
(transient-define-infix magit-commit:--cz-scope ()
  :description "Commit scope"
  :class 'magit--git-variable
  :reader 'read-string
  :variable "commit.czScope")

(transient-append-suffix 'magit-commit "c" '("B" "Breaking Change"  magit-cz--breaking-change-toggle))
(transient-append-suffix 'magit-commit "-D" '("=t" magit-commit:--cz-type))
(transient-append-suffix 'magit-commit "-D" '("=s" magit-commit:--cz-scope))

;; (setq magit-process-prompt-functions '())
;; (add-to-list 'magit-process-prompt-functions 'magit-cz-functions-detect-cz)
;; (add-to-list 'magit-process-prompt-functions 'magit-cz-functions-cz-types)
;; (add-to-list 'magit-process-prompt-functions 'magit-cz-functions-cz-scope)
;; (add-to-list 'magit-process-prompt-functions 'magit-cz-functions-cz-short-msg)
;; (add-to-list 'magit-process-prompt-functions 'magit-cz-functions-cz-long-msg)
;; (add-to-list 'magit-process-prompt-functions 'magit-cz-functions-cz-breaking-change-p)
;; (add-to-list 'magit-process-prompt-functions 'magit-cz-functions-cz-breaking-change-body)
;; (add-to-list 'magit-process-prompt-functions 'magit-cz-functions-cz-breaking-change-des)
;; (add-to-list 'magit-process-prompt-functions 'magit-cz-functions-cz-issue-p)
;; (add-to-list 'magit-process-prompt-functions 'magit-cz-functions-cz-issue-num)

(provide 'magit-cz)
;;; magit-cz.el ends here