;;; sql.el ---   -*- lexical-binding: t; -*-

;; Copyright © 2019, Rashawn Zhang, all rights reserved.

;; Author: Rashawn Zhang <hi@yqrashawn.com>
;; Created: 24 April 2019
;; Keywords:

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
(defun spacemacs/sql-populate-products-list (&rest args)
  "Update Spacemacs list of sql products"
  (setq
   spacemacs-sql-highlightable sql-product-alist
   spacemacs-sql-startable (remove-if-not
                            (lambda (product) (sql-get-product-feature (car product) :sqli-program))
                            sql-product-alist)))

(use-package sql
  :defer t
  :init (spacemacs/register-repl 'sql 'spacemacs/sql-start "sql")
  :config
  (progn
    (setq
     ;; should not set this to anything else than nil
     ;; the focus of SQLi is handled by spacemacs conventions
     sql-pop-to-buffer-after-send-region nil)
    (advice-add 'sql-add-product :after #'spacemacs/sql-populate-products-list)
    (advice-add 'sql-del-product :after #'spacemacs/sql-populate-products-list)
    (spacemacs/sql-populate-products-list)

    (defun spacemacs//sql-source (products)
      "return a source for helm selection"
      `((name . "SQL Products")
        (candidates . ,(mapcar (lambda (product)
                                 (cons (sql-get-product-feature (car product) :name)
                                       (car product)))
                               products))
        (action . (lambda (candidate) (helm-marked-candidates)))))

    (defun spacemacs/sql-highlight ()
      "set SQL dialect-specific highlighting"
      (interactive)
      (let ((product (car (helm
                           :sources (list (spacemacs//sql-source spacemacs-sql-highlightable))))))
        (sql-set-product product)))

    (defun spacemacs/sql-start ()
      "set SQL dialect-specific highlighting and start inferior SQLi process"
      (interactive)
      (let ((product (car (helm
                           :sources (list (spacemacs//sql-source spacemacs-sql-startable))))))
        (sql-set-product product)
        (sql-product-interactive product)))

    (defun spacemacs/sql-send-string-and-focus ()
      "Send a string to SQLi and switch to SQLi in `insert state'."
      (interactive)
      (let ((sql-pop-to-buffer-after-send-region t))
        (call-interactively 'sql-send-string)
        (evil-insert-state)))

    (defun spacemacs/sql-send-buffer-and-focus ()
      "Send the buffer to SQLi and switch to SQLi in `insert state'."
      (interactive)
      (let ((sql-pop-to-buffer-after-send-region t))
        (sql-send-buffer)
        (evil-insert-state)))

    (defun spacemacs/sql-send-paragraph-and-focus ()
      "Send the paragraph to SQLi and switch to SQLi in `insert state'."
      (interactive)
      (let ((sql-pop-to-buffer-after-send-region t))
        (sql-send-paragraph)
        (evil-insert-state)))

    (defun spacemacs/sql-send-region-and-focus (start end)
      "Send region to SQLi and switch to SQLi in `insert state'."
      (interactive "r")
      (let ((sql-pop-to-buffer-after-send-region t))
        (sql-send-region start end)
        (evil-insert-state)))

    (evil-define-key 'normal sql-mode-map
      ",'" 'spacemacs/sql-start

      ;; sqli buffer
      ",bb" 'sql-show-sqli-buffer
      ",bs" 'sql-set-sqli-buffer

      ;; dialects
      ",hk" 'spacemacs/sql-highlight

      ;; interactivity
      ",sb" 'sql-send-buffer
      ",sB" 'spacemacs/sql-send-buffer-and-focus
      ",si" 'spacemacs/sql-start
      ;; paragraph gets "f" here because they can be assimilated to functions.
      ;; If you separate your commands in a SQL file, this key will send the
      ;; command around point, which is what you probably want.
      ",sf" 'sql-send-paragraph
      ",sF" 'spacemacs/sql-send-paragraph-and-focus
      ",sq" 'sql-send-string
      ",sQ" 'spacemacs/sql-send-string-and-focus
      ",sr" 'sql-send-region
      ",sR" 'spacemacs/sql-send-region-and-focus

      ;; listing
      ",la" 'sql-list-all
      ",lt" 'sql-list-table)

    (spacemacs/set-leader-keys-for-major-mode 'sql-interactive-mode
      ;; sqli buffer
      ",br" 'sql-rename-buffer
      ",bS" 'sql-save-connection)

    (add-hook 'sql-interactive-mode-hook
              (lambda () (toggle-truncate-lines t)))))

(use-package ob-sql-mode
  :straight t
  :defer t)
(use-package sqlformat
  :straight t
  :hook (sql-mode . sqlformat-on-save-mode))

;;; sql.el ends here