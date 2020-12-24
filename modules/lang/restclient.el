;;; restclient.el ---  config of restclient package -*- lexical-binding: t; -*-

;; Copyright © 2020, Rashawn Zhang, all rights reserved.

;; Author: Rashawn Zhang <namy.19@gmail.com>
;; Created: 28 May 2018
;; Keywords: restclient

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

;;  config of restclient package

;;; Code:
(setq restclient-use-org nil)
(use-package restclient
  :straight t
  :defer t
  :init
  (unless restclient-use-org
    (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))
  :config
  (evil-define-key 'normal restclient-mode-map
    ",n" 'restclient-jump-next
    ",p" 'restclient-jump-prev
    ",s" 'restclient-http-send-current-stay-in-window
    ",S" 'restclient-http-send-current
    ",r" 'spacemacs/restclient-http-send-current-raw-stay-in-window
    ",R" 'restclient-http-send-current-raw
    ",y" 'restclient-copy-curl-command))


(provide 'restclient)
;;; restclient.el ends here