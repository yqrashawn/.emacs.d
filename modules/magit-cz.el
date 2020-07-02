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
(require 'dash)

(defgroup magit-cz nil
  "Commitizen support for magit")

(defvar magit-cz--messge nil)

(defcustom magit-cz-types
  '((?c "chore" "Other changes that don't modify src or test files" "Chores")
    (?F "feat" "New Feature" "Features")
    (?f "fix" "A bug fix" "Bug Fixes")
    (?t "test" "Adding missing tests or correcting existing tests" "Tests")
    (?d "docs" "Documentation only changes" "Documentation")
    (?s "style" "Changes that do not affect the meaning of the code (white-space formatting missing semi-colons etc)" "Styles")
    (?r "refactor" "A code change that neither fixes a bug nor adds a feature" "Code Refactoring")
    (?p "perf" "A code change that improves performance" "Performance Improvements")
    (?b "build" "Changes that affect the build system or external dependencies (example scopes gulp broccoli npm)" "Builds")
    (?C "ci" "Changes to our CI configuration files and scripts (example scopes Travis Circle BrowserStack SauceLabs)" "Continuous Integrations")
    (?r "revert" "Reverts a previous commit" "Reverts")
    (?q "quit" "Do nothing" "Quit")
    (? " quit" "Do nothing" "Quit"))
  "Commit type from `conventional-commit-types' npm package"
  :group 'magit-cz)

(defcustom magit-cz-types-prompt-template "[%s]%s - %s"
  "Template for types prompt")

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

(defun magit-cz-git-commit-message-setup-function ()
  (insert (or magit-cz--message ""))
  (goto-char (point-min))
  (end-of-line)
  (setq magit-cz--message nil))

(add-hook 'git-commit-setup-hook 'magit-cz-git-commit-message-setup-function)

(defun magit-cz-functions-detect-and-cancel-cz (proc s)
    (when (or (string-match-p "cz-cli" s)
              (string-match-p "cz-conventional-changelog" s))
      (process-send-string proc "")
      t))

(add-to-list 'magit-process-prompt-functions 'magit-cz-functions-detect-and-cancel-cz)

(defun magit-cz-git-commit-setup-function (bc?)
  (let* ((type (car (alist-get (read-char-choice (magit-cz--types-message) (mapcar 'car magit-cz-types)) magit-cz-types)))
         (type? (not (or (string= type " quit") (string= type "quit")))))
    (when type?
      (let* ((scope (read-string "Commit scope: "))
             (scope? (not (eq (length scope) 0)))
             (bc-body (and bc? (read-string "Breaking Change Body: ")))
             (msg (if scope? (format "%s(%s): " type scope) (concat type ": ")))
             (msg (if bc? (format "%s\n\n\nBREAKING CHANGE: %s\n" msg bc-body) msg)))
        msg))))

(defun magit-cz-commit (&optional args)
  (interactive)
  (setq magit-cz--message (magit-cz-git-commit-setup-function nil))
  (magit-commit-create args))

(defun magit-cz-breaking-change-commit (&optional args)
  (interactive)
  (setq magit-cz--message (magit-cz-git-commit-setup-function t))
  (magit-commit-create args))

(transient-append-suffix 'magit-commit "c" '("c" "CZ Commit"  magit-cz-commit))
(transient-append-suffix 'magit-commit "c" '("v" "Magit Commit"  magit-commit-create))
(transient-append-suffix 'magit-commit "v" '("B" "CZ Breaking Change Commit"  magit-cz-breaking-change-commit))

(defvar magit-cz--debug nil)

;;; handle husky, skip husky if there's --no-verify arg
(defun magit-cz--maybe-disable-husky (&rest args)
  (print args)
  (when (-contains? (-flatten args) "--no-verify")
    (and magit-cz--debug (message "husky 1"))
    (setenv "HUSKY_SKIP_HOOKS" "1")))

(defun magit-cz--maybe-enable-husky (&rest args)
  (when (-contains? (-flatten args) "--no-verify")
    (and magit-cz--debug (message "husky 0"))
    (setenv "HUSKY_SKIP_HOOKS")))

(advice-add 'magit-run-git-sequencer :before 'magit-cz--maybe-disable-husky)
(advice-add 'magit-run-git-sequencer :after 'magit-cz--maybe-enable-husky)

;; (advice-remove 'magit-run-git-sequencer 'magit-cz--maybe-enable-husky)
;; (advice-remove 'magit-run-git-sequencer 'magit-cz--maybe-disable-husky)

(provide 'magit-cz)
;;; magit-cz.el ends here