;;; clojure.el ---  clojure packages -*- lexical-binding: t; -*-

;; Copyright © 2018, Rashawn Zhang, all rights reserved.

;; Author: Rashawn Zhang <namy.19@gmail.com>
;; Created: 18 April 2018
;; Keywords: clojure

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

;;  clojure packages

;;; Code:
(yq/get-modules "lang/clojure-funcs.el")

(use-package clojure-mode
  :straight t
  :diminish clojure-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
  ;; This regexp matches shebang expressions like `#!/usr/bin/env boot'
  (add-to-list 'magic-mode-alist '("#!.*boot\\s-*$" . clojure-mode))
  :config
  (dolist (map (list clojure-mode-map clojurec-mode-map clojurescript-mode-map))
    (evil-define-key* 'normal map
                      ",fl" 'clojure-align))
  (when clojure-enable-fancify-symbols
    (dolist (m '(clojure-mode clojurescript-mode clojurec-mode))
      (clojure/fancify-symbols m))))

(use-package cider
  :straight t
  :defer t
  :init
  (spacemacs|add-company-backends
    :backends company-capf
    :modes
    cider-mode
    cider-repl-mode)
  (setq cider-default-cljs-repl 'figwheel)
  (spacemacs|define-jump-handlers clojure-mode)
  (add-to-list (intern (format "spacemacs-jump-handlers-%S" 'clojure-mode))
               '(cider-find-dwim :async t))
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'cider-clojure-interaction-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
  (spacemacs/register-repl 'cider 'cider-jack-in "cider")
  (evil-define-key 'normal clojure-mode-map ",c" 'cider-cheatsheet)

  (setq cider-stacktrace-default-filters '(tooling dup)
        cider-repl-pop-to-buffer-on-connect nil
        cider-repl-display-in-current-window t
        ;; cider-repl-display-in-current-window nil
        cider-prompt-save-file-on-load nil
        cider-auto-select-error-buffer nil
        cider-eval-result-prefix ";; => "
        cider-repl-result-prefix ";; => "
        cider-repl-use-clojure-font-lock t
        cider-repl-use-pretty-printing t
        cider-repl-wrap-history t
        cider-repl-history-quit-action 'delete-and-restore
        cider-repl-history-show-preview t
        cider-repl-history-display-duplicates nil
        cider-repl-history-highlight-inserted-item t
        cider-repl-history-file (concat spacemacs-cache-directory "cider-repl-history"))
  (add-hook 'clojure-mode-hook 'cider-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (dolist (x '(spacemacs-jump-handlers-clojure-mode
               spacemacs-jump-handlers-clojurec-mode
               spacemacs-jump-handlers-clojurescript-mode
               spacemacs-jump-handlers-clojurex-mode
               spacemacs-jump-handlers-cider-repl-mode))
    (add-to-list x 'spacemacs/clj-find-var))
  (add-to-list 'evil-insert-state-modes 'cider-repl-mode)
  (add-to-list 'evil-insert-state-modes 'cider--debug-mode)
  (add-hook 'clojure-mode-hook #'spacemacs//init-jump-handlers-clojure-mode)
  (add-hook 'clojurescript-mode-hook #'spacemacs//init-jump-handlers-clojurescript-mode)
  (add-hook 'clojurec-mode-hook #'spacemacs//init-jump-handlers-clojurec-mode)
  (add-hook 'cider-repl-mode-hook #'spacemacs//init-jump-handlers-cider-repl-mode)
  :config
  ;; TODO: having this work for cider-macroexpansion-mode would be nice,
  ;;       but the problem is that it uses clojure-mode as its major-mode
  (dolist (map (list clojure-mode-map
                     clojurec-mode-map
                     clojurescript-mode-map
                     cider-repl-mode-map))

    (evil-define-key* 'normal map
                      ",ha" 'cider-apropos
                      ",hc" 'clojure-cheatsheet
                      ",hg" 'cider-grimoire
                      ",hh" 'cider-doc
                      ",hj" 'cider-javadoc
                      ",hn" 'cider-browse-ns

                      ",e;" 'cider-eval-defun-to-comment
                      ",eb" 'cider-eval-buffer
                      ",ee" 'cider-eval-last-sexp
                      ",ef" 'cider-eval-defun-at-point
                      ",em" 'cider-macroexpand-1
                      ",eM" 'cider-macroexpand-all
                      ",eP" 'cider-pprint-eval-last-sexp
                      ",er" 'cider-eval-region
                      ",ew" 'cider-eval-last-sexp-and-replace

                      ",="  'cider-format-buffer
                      ",fb" 'cider-format-buffer

                      ",gb" 'cider-pop-back
                      ",gc" 'cider-classpath
                      ",ge" 'cider-jump-to-compilation-error
                      ",gn" 'cider-browse-ns
                      ",gN" 'cider-browse-ns-all
                      ",gr" 'cider-find-resource
                      ",gs" 'cider-browse-spec
                      ",gS" 'cider-browse-spec-all

                      ",'"  'cider-jack-in
                      ",\"" 'cider-jack-in-clojurescript
                      ",sb" 'cider-load-buffer
                      ",sB" 'spacemacs/cider-send-buffer-in-repl-and-focus
                      ",sc" (if (eq map 'cider-repl-mode)
                                'cider-repl-clear-buffer
                              'cider-connect)
                      ",sC" 'cider-find-and-clear-repl-output
                      ",se" 'spacemacs/cider-send-last-sexp-to-repl
                      ",sE" 'spacemacs/cider-send-last-sexp-to-repl-focus
                      ",sf" 'spacemacs/cider-send-function-to-repl
                      ",sF" 'spacemacs/cider-send-function-to-repl-focus
                      ",si" 'cider-jack-in
                      ",sI" 'cider-jack-in-clojurescript
                      ",sn" 'spacemacs/cider-send-ns-form-to-repl
                      ",sN" 'spacemacs/cider-send-ns-form-to-repl-focus
                      ",so" 'cider-repl-switch-to-other
                      ",sq" 'cider-quit
                      ",sr" 'spacemacs/cider-send-region-to-repl
                      ",sR" 'spacemacs/cider-send-region-to-repl-focus
                      ",ss" (if (eq map 'cider-repl-mode)
                                'cider-switch-to-last-clojure-buffer
                              'cider-switch-to-repl-buffer)
                      ",sx" 'cider-refresh
                      ",sX" 'cider-restart

                      ",Te" 'cider-enlighten-mode
                      ",Tf" 'spacemacs/cider-toggle-repl-font-locking
                      ",Tp" 'spacemacs/cider-toggle-repl-pretty-printing
                      ",Tt" 'cider-auto-test-mode

                      ",ta" 'spacemacs/cider-test-run-all-tests
                      ",tb" 'cider-test-show-report
                      ",tl" 'spacemacs/cider-test-run-loaded-tests
                      ",tn" 'spacemacs/cider-test-run-ns-tests
                      ",tp" 'spacemacs/cider-test-run-project-tests
                      ",tr" 'spacemacs/cider-test-rerun-failed-tests
                      ",tt" 'spacemacs/cider-test-run-focused-test

                      ",db" 'cider-debug-defun-at-point
                      ",de" 'spacemacs/cider-display-error-buffer
                      ",dv" 'cider-inspect

                      ;; profile
                      ",p+" 'cider-profile-samples
                      ",pc" 'cider-profile-clear
                      ",pn" 'cider-profile-ns-toggle
                      ",ps" 'cider-profile-var-summary
                      ",pS" 'cider-profile-summary
                      ",pt" 'cider-profile-toggle
                      ",pv" 'cider-profile-var-profiled-p

                      ;; refactorings from clojure-mode
                      ",rc#" 'clojure-convert-collection-to-set
                      ",rc'" 'clojure-convert-collection-to-quoted-list
                      ",rc(" 'clojure-convert-collection-to-list
                      ",rc:" 'clojure-toggle-keyword-string
                      ",rc[" 'clojure-convert-collection-to-vector
                      ",rc{" 'clojure-convert-collection-to-map
                      ))
  ;; (add-hook 'cider-connected-hook (lambda ()
  ;;                                   (interactive)
  ;;                                   (cider-load-file
  ;;                                    (expand-file-name "lispy-clojure.clj" lispy-site-directory))))
  ;; cider-repl-mode only
  (define-key cider-repl-mode-map (kbd "C-c C-l") 'cider-repl-clear-buffer)
  (evil-define-key 'normal cider-repl-mode-map ",," 'cider-repl-handle-shortcut)
  (evil-define-key 'insert cider-repl-mode-map (kbd "RET" ) 'cider-repl-closing-return)
  (evil-define-key 'insert cider-repl-mode-map (kbd "C-n" ) 'cider-repl-next-input)
  (evil-define-key 'insert cider-repl-mode-map (kbd "C-p" ) 'cider-repl-previous-input)
  (evil-define-key 'insert cider-repl-mode-map (kbd "<C-return>" ) 'newline-and-indent)
  :config
  ;; add support for golden-ratio
  (with-eval-after-load 'golden-ratio
    (push 'cider-popup-buffer-quit-function golden-ratio-extra-commands))
  ;; add support for evil
  (evil-set-initial-state 'cider-stacktrace-mode 'motion)
  (evil-set-initial-state 'cider-popup-buffer-mode 'motion)
  (add-hook 'cider--debug-mode-hook 'spacemacs/cider-debug-setup)

  (evilified-state-evilify cider-stacktrace-mode cider-stacktrace-mode-map
    (kbd "C-j") 'cider-stacktrace-next-cause
    (kbd "C-k") 'cider-stacktrace-previous-cause
    (kbd "TAB") 'cider-stacktrace-cycle-current-cause
    (kbd "0")   'cider-stacktrace-cycle-all-causes
    (kbd "1")   'cider-stacktrace-cycle-cause-1
    (kbd "2")   'cider-stacktrace-cycle-cause-2
    (kbd "3")   'cider-stacktrace-cycle-cause-3
    (kbd "4")   'cider-stacktrace-cycle-cause-4
    (kbd "5")   'cider-stacktrace-cycle-cause-5
    (kbd "a")   'cider-stacktrace-toggle-all
    (kbd "c")   'cider-stacktrace-toggle-clj
    (kbd "d")   'cider-stacktrace-toggle-duplicates
    (kbd "J")   'cider-stacktrace-toggle-java
    (kbd "r")   'cider-stacktrace-toggle-repl
    (kbd "T")   'cider-stacktrace-toggle-tooling)

  ;; open cider-doc directly and close it with q
  (setq cider-prompt-for-symbol nil)

  (evilified-state-evilify cider-docview-mode cider-docview-mode-map
    (kbd "q") 'cider-popup-buffer-quit)

  (add-hook 'cider-inspector-mode-hook 'visual-line-mode)
  (evilified-state-evilify cider-inspector-mode cider-inspector-mode-map
    (kbd "L") 'cider-inspector-pop
    (kbd "n") 'cider-inspector-next-page
    (kbd "N") 'cider-inspector-prev-page
    (kbd "p") 'cider-inspector-prev-page
    (kbd "r") 'cider-inspector-refresh)

  (evilified-state-evilify cider-test-report-mode cider-test-report-mode-map
    (kbd "C-j") 'cider-test-next-result
    (kbd "C-k") 'cider-test-previous-result
    (kbd "RET") 'cider-test-jump
    (kbd "d")   'cider-test-ediff
    (kbd "e")   'cider-test-stacktrace
    (kbd "q")   'cider-popup-buffer-quit
    (kbd "r")   'cider-test-rerun-tests
    (kbd "t")   'cider-test-run-test
    (kbd "T")   'cider-test-run-ns-tests)

  (evil-define-key* 'normal cider-repl-mode-map
                    (kbd "C-j") 'cider-repl-next-input
                    (kbd "C-k") 'cider-repl-previous-input)

  (when clojure-enable-fancify-symbols
    (clojure/fancify-symbols 'cider-repl-mode)
    (clojure/fancify-symbols 'cider-clojure-interaction-mode))

  (defadvice cider-jump-to-var (before add-evil-jump activate)
    (evil-set-jump)))

(use-package clj-refactor
  :straight t
  :diminish clj-refactor-mode
  :defer t
  :init
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-f")

  ;; Usually we do not set keybindings in :config, however this must be done
  ;; here because it reads the variable `cljr--all-helpers'. Since
  ;; `clj-refactor-mode' is added to the hook, this should trigger when a
  ;; clojure buffer is opened anyway, so there's no "keybinding delay".
  (let ((clj-refactor--key-binding-prefixes
         '()))
    (dolist (map '(clojure-mode-map
                   clojurec-mode-map
                   clojurescript-mode-map
                   cider-repl-mode-map))
      (dolist (r cljr--all-helpers)
        (let* ((binding (car r))
               (func (cadr r)))
          (when (not (string-prefix-p "hydra" (symbol-name func)))
            (evil-define-key 'normal map
              (concat ",r" binding) func)))))))

;; (use-package helm-cider
;;   :straight t
;;   :commands (helm-cider-mode)
;;   :hook ((clojure-mode cider-repl-mode cider-docview-mode cider-inspector-mode) . helm-cider-mode))

(use-package cider-eval-sexp-fu
  :straight t
  :after cider)

(use-package clojure-snippets
  :straight t
  :after clojure-mode)

(use-package flycheck-clojure
  :straight t
  :after (flycheck cider))

(use-package clojure-mode-extra-font-locking
  :straight t
  :after clojure-mode)

;; (use-package clojure-cheatsheet
;;   :straight t
;;   :defer t
;;   :commands (clojure-cheatsheet)
;;   :config
;;   (dolist (map (list clojure-mode-map
;;                      clojurec-mode-map
;;                      clojurescript-mode-map
;;                      cider-repl-mode-map))
;;     (evil-define-key 'normal map
;;       ",hc" 'clojure-cheatsheet)))

(use-package clojure-snippets
  :defer t)

(use-package sayid
  :straight t
  :defer t
  :init
  (setq sayid--key-binding-prefixes
        '(("mdt" . "trace")))

  (evilified-state-evilify sayid-mode sayid-mode-map
    (kbd "H") 'sayid-buf-show-help
    (kbd "n") 'sayid-buffer-nav-to-next
    (kbd "N") 'sayid-buffer-nav-to-prev
    (kbd "C-s v") 'sayid-toggle-view
    (kbd "C-s V") 'sayid-set-view
    (kbd "L") 'sayid-buf-back
    (kbd "e") 'sayid-gen-instance-expr) ;Originally this was bound to 'g', but I feel this is still mnemonic and doesn't overlap with evil

  (evilified-state-evilify sayid-pprint-mode sayid-pprint-mode-map
    (kbd "h") 'sayid-pprint-buf-show-help
    (kbd "n") 'sayid-pprint-buf-next
    (kbd "N") 'sayid-pprint-buf-prev
    (kbd "l") 'sayid-pprint-buf-exit)

  (evilified-state-evilify sayid-traced-mode sayid-traced-mode-map
    (kbd "l") 'sayid-show-traced
    (kbd "h") 'sayid-traced-buf-show-help)
  :config
  (dolist (map (list clojure-mode-map
                     clojurec-mode-map
                     clojurescript-mode-map
                     cider-repl-mode-map))
    (evil-define-key* 'normal map
                      ;;These keybindings mostly preserved from the default sayid bindings
                      ",d!" 'sayid-load-enable-clear
                      ",dE" 'sayid-eval-last-sexp ;in default sayid bindings this is lowercase e, but that was already used in clojure mode
                      ",dc" 'sayid-clear-log
                      ",df" 'sayid-query-form-at-point
                      ",dh" 'sayid-show-help
                      ",ds" 'sayid-show-traced
                      ",dS" 'sayid-show-traced-ns
                      ",dtb" 'sayid-trace-ns-in-file
                      ",dtd" 'sayid-trace-fn-disable
                      ",dtD" 'sayid-trace-disable-all
                      ",dte" 'sayid-trace-fn-enable
                      ",dtE" 'sayid-trace-enable-all
                      ",dtK" 'sayid-kill-all-traces
                      ",dtn" 'sayid-inner-trace-fn
                      ",dto" 'sayid-outer-trace-fn
                      ",dtp" 'sayid-trace-ns-by-pattern
                      ",dtr" 'sayid-remove-trace-fn
                      ",dty" 'sayid-trace-all-ns-in-dir
                      ",dV" 'sayid-set-view
                      ",dw" 'sayid-get-workspace
                      ",dx" 'sayid-reset-workspace
                      )))
(provide 'clojure)
;;; clojure.el ends here
