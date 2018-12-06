;;; lua.el --- lua packages  -*- lexical-binding: t; -*-

;; Copyright © 2018, Rashawn Zhang, all rights reserved.

;; Author: Rashawn Zhang <namy.19@gmail.com>
;; Created: 15 April 2018
;; Keywords: lua

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

;;  lua packages

;;; Code:

(use-package lua-mode
  :straight t
  :mode ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode)
  :init
  (setq lua-indent-level 2
        lua-indent-string-contents t)
  (evil-define-key 'normal lua-mode-map ",d" 'lua-search-documentation)
  (evil-define-key 'normal lua-mode-map ",sb" 'lua-send-buffer)
  (evil-define-key 'normal lua-mode-map ",sf" 'lua-send-defun)
  (evil-define-key 'normal lua-mode-map ",sl" 'lua-send-current-line)
  (evil-define-key 'normal lua-mode-map ",sr" 'lua-send-region))

(use-package company-lua
  :straight t
  :after lua-mode
  :init
  (add-hook 'lua-mode-hook
            (lambda () (spacemacs|add-company-backends
                         :backends company-lua
                         :modes lua-mode
                         :after-hook t))))

(provide 'lua)
;;; lua.el ends here