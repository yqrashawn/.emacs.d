;;; clojure.el ---  clojure packages -*- lexical-binding: t; -*-

;; Copyright © 2020, Rashawn Zhang, all rights reserved.

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

;; (dolist (mode '(clojure-mode clojurescript-mode clojurec-mode))
;;     (spacemacs/enable-flycheck mode))

(use-package clojure-mode
  :straight t
  :diminish (clojure-mode
             clojurec-mode
             clojurescript-mode
             clojurex-mode)
  :magic ("^#![^\n]*/\\(clj\\|clojure\\|bb\\|lumo\\)" . clojure-mode)
  :custom
  (clojure-align-forms-automatically nil)
  (clojure-verify-major-mode nil)
  (clojure-toplevel-inside-comment-form t)
  (clojure-align-reader-conditionals t)
  (clojure-defun-indents '(fn-traced))
  :init
  (defun +setup-company-for-clojure ()
    (setq-local company-idle-delay 0.2)
    (setq-local evil-shift-width 1)
    (setq-local company-backends '(company-capf (company-dabbrev-code company-gtags company-etags company-keywords) company-files company-dabbrev)))
  (add-hook! clojure-mode
    (defl () (require 'mode-local) (setq-mode-local clojure-mode company-idle-delay 0.2))
    #'spacemacs//init-jump-handlers-clojure-mode)
  (add-hook! clojurec-mode #'spacemacs//init-jump-handlers-clojurec-mode)
  (add-hook! clojurescript-mode #'spacemacs//init-jump-handlers-clojurescript-mode)
  (add-hook! (clojure-mode clojurescript-mode clojurec-mode) '+setup-company-for-clojure)
  (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
  ;; This regexp matches shebang expressions like `#!/usr/bin/env boot'
  (add-to-list 'magic-mode-alist '("#!.*boot\\s-*$" . clojure-mode))
  :config
  (dolist (map (list clojure-mode-map clojurescript-mode-map clojurec-mode-map))
    (print map)
    (evil-define-key* 'normal map
      ;; refactorings from clojure-mode
      ",rc#" 'clojure-convert-collection-to-set
      ",rc'" 'clojure-convert-collection-to-quoted-list
      ",rc(" 'clojure-convert-collection-to-list
      ",rc:" 'clojure-toggle-keyword-string
      ",rc[" 'clojure-convert-collection-to-vector
      ",rc{" 'clojure-convert-collection-to-map
      ",fl" 'clojure-align))

  ;; #_ is not a logical sexp
  (defadvice! corgi/clojure--looking-at-non-logical-sexp (command)
    :around #'clojure--looking-at-non-logical-sexp
    "Return non-nil if text after point is \"non-logical\" sexp.
\"Non-logical\" sexp are ^metadata and #reader.macros."
    (comment-normalize-vars)
    (comment-forward (point-max))
    (looking-at-p "\\(?:#?\\^\\)\\|#:?:?[[:alpha:]]\\|#_"))
  (when clojure-enable-fancify-symbols
    (dolist (m '(clojure-mode clojurescript-mode clojurec-mode))
      (clojure/fancify-symbols m)
      ;; (spacemacs/enable-flycheck m)
      )))

(use-package cider
  :straight (:host :github :repo "clojure-emacs/cider")
  :hook ((clojure-mode clojurescript-mode clojurec-mode) . cider-mode)
  :custom
  (cider-inspector-page-size 64)
  (cider-inspector-max-atom-length 1000)
  (cider-inspector-max-coll-size 20)
  (cider-default-cljs-repl 'shadow)
  (cider-preferred-build-tool 'clojure-cli)
  (nrepl-log-messages t)
  (cider-completion-annotations-include-ns 'always)
  (cider-connection-message-fn 'cider-random-tip)
  (cider-eldoc-display-context-dependent-info t)
  (cider-print-fn 'fipp)
  (cider-print-quota (* 1024 1024 10))  ;; 10MB
  (cider-print-buffer-size (* 1024 4))
  (cider-debug-display-locals t)
  (cider-special-mode-truncate-lines nil)
  (cider-debug-display-locals t)
  (cider-repl-wrap-history t)
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-stacktrace-default-filters '(tooling dup))
  (cider-repl-display-in-current-window t)
  (cider-save-file-on-load nil)
  (cider-ns-save-files-on-refresh nil)
  (nrepl-sync-request-timeout 3)
  (cider-font-lock-dynamically '(macro core function var deprecated))
  ;; (cider-font-lock-dynamically nil)
  ;; don't kill the REPL when printing large data structures
  (cider-print-options '(("length" 80) ("level" 20) ("right-margin" 80)))
  (cider-auto-select-error-buffer nil)
  (cider-eval-result-prefix ";; => ")
  (cider-repl-result-prefix ";; => ")
  (cider-repl-use-clojure-font-lock t)
  (cider-repl-use-pretty-printing t)
  (cider-repl-wrap-history t)
  (cider-repl-history-quit-action 'delete-and-restore)
  (cider-repl-history-show-preview t)
  (cider-repl-history-display-duplicates nil)
  (cider-repl-history-highlight-inserted-item 'pulse)
  (cider-repl-history-file (concat spacemacs-cache-directory "cider-repl-history"))
  (nrepl-hide-special-buffers nil)
  :init
  ;; (customize-set-variable 'cider-default-repl-command 'lein)
  (spacemacs|add-company-backends
    :backends (company-capf)
    :modes
    cider-mode
    cider-repl-mode
    :after-hook t)
  (spacemacs|define-jump-handlers clojure-mode)
  (with-eval-after-load 'clojure-mode
    (add-to-list (intern (format "spacemacs-jump-handlers-%S" 'clojure-mode))
                 '(cider-find-dwim :async t)))
  (spacemacs/register-repl 'cider 'cider-jack-in "cider")
  (evil-define-key 'normal clojure-mode-map ",c" 'cider-cheatsheet)
  (with-eval-after-load 'cojure-mode
    (dolist (x '(spacemacs-jump-handlers-clojure-mode
                 spacemacs-jump-handlers-clojurec-mode
                 spacemacs-jump-handlers-clojurescript-mode
                 spacemacs-jump-handlers-clojurex-mode
                 spacemacs-jump-handlers-cider-repl-mode))
      (add-to-list x 'spacemacs/clj-find-var)))
  (pushnew! evil-insert-state-modes 'cider-repl-mode)
  (add-hook! 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook! 'cider-repl-mode-hook
             #'spacemacs//init-jump-handlers-cider-repl-mode
             #'cider-company-enable-fuzzy-completion
             '+setup-company-for-clojure)
  (defun +bind-clj-key (maps)
    (dolist (map maps)
      (evil-define-key* 'normal map
        ",c" 'cider-cheatsheet
        ",qr" 'sesman-restart
        ",qq" 'sesman-quit
        ",ha" 'cider-apropos
        ",hc" 'cider-clojuredocs
        ",hw" 'cider-clojuredocs-web
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

        ",=" 'cider-format-buffer
        ",fb" 'cider-format-buffer

        ",gb" 'cider-pop-back
        ",gc" 'cider-classpath
        ",ge" 'cider-jump-to-compilation-error
        ",gn" 'cider-find-ns
        ",gN" 'cider-browse-ns-all
        ",gr" 'cider-find-resource
        ",gs" 'cider-browse-spec
        ",gS" 'cider-browse-spec-all
        ;; find deps function of current function
        ",gd" 'cider-find-dwim
        ;; ",gd" 'cider-xref-fn-deps-select
        ;; find current function usage
        ",gu" 'cider-xref-fn-refs-select

        ",'" 'cider-jack-in
        ",\"" 'cider-jack-in-clojurescript
        ",ja" 'cider-jack-in-clj&cljs
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
        ",sx" 'cider-ns-refresh
        ",sX" 'cider-restart
        ",rn" 'cider-ns-reload
        ",rN" 'cider-ns-reload-all

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

        ",tv" 'cider-toggle-trace-var
        ;; ",tn" 'cider-toggle-trace-ns
        ",db" 'cider-debug-defun-at-point
        ",de" 'spacemacs/cider-display-error-buffer
        ",dv" 'cider-inspect
        ",il" 'cider-inspect-last-result
        ",is" 'cider-inspect-last-sexp
        ",iD" 'cider-inspect-defun-at-point
        ",bs" 'cider-browse-spec
        ",bS" 'cider-browse-spec-all

        ;; profile
        ",p+" 'cider-profile-samples
        ",pc" 'cider-profile-clear
        ",pn" 'cider-profile-ns-toggle
        ",ps" 'cider-profile-var-summary
        ",pS" 'cider-profile-summary
        ",pt" 'cider-profile-toggle
        ",pv" 'cider-profile-var-profiled-p)))

  ;; https://philjackson.github.io/emacs/evil/cider/debugging/2021/06/06/using-the-cider-debugger-in-evil/
  (defun my-cider-debug-toggle-insert-state ()
    (if cider--debug-mode   ;; Checks if you're entering the debugger
        (evil-insert-state) ;; If so, turn on evil-insert-state
      (evil-normal-state))) ;; Otherwise, turn on normal-state

  (defun corgi/cider-quit-all ()
    "Quit all current CIDER REPLs."
    (interactive)
    (let ((repls (seq-remove (lambda (r)
                               (equal r (get-buffer "*babashka-repl*")))
                             (seq-mapcat #'cdr (sesman-current-sessions 'CIDER)))))
      (seq-do #'cider--close-connection repls))
    ;; if there are no more sessions we can kill all ancillary buffers
    (cider-close-ancillary-buffers)
    ;; need this to refresh sesman browser
    (run-hooks 'sesman-post-command-hook))

  :config
  (with-eval-after-load 'clojure-mode (+bind-clj-key (list clojure-mode-map clojurec-mode-map clojurescript-mode-map)))
  (+bind-clj-key (list cider-repl-mode-map))
  (defadvice! +cider-add-to-alist (orig-fn alist name version)
    :around #'cider-add-to-alist
    (if (not (= alist 'cider-jack-in-dependencies))
        (apply orig-fn alist name version)
      (cond ((string= name "com.cemerick/pomegranate")
             (apply orig-fn alist "clj-commons/pomegranate" "1.2.1"))
            (t (apply orig-fn alist name version)))))
  ;; When asking for a "matching" REPL (clj/cljs), and no matching REPL is found,
  ;; return any REPL that is there. This is so that cider-quit can be called
  ;; repeatedly to close all REPLs in a process. It also means that , s s will go
  ;; to any REPL if there is one open.
  (defadvice! corgi/around-cider-current-repl (command &optional type ensure)
    :around #'cider-current-repl
    (let ((repl (or
                 (if (not type)
                     (or (funcall command nil)
                         (funcall command 'any))
                   (funcall command type))
                 (get-buffer "*babashka-repl*"))))
      (if (and ensure (null repl))
          (cider--no-repls-user-error type)
        repl)))
  ;; This essentially redefines cider-repls. The main thing it does is return all
  ;; REPLs by using sesman-current-sessions (plural) instead of
  ;; sesman-current-session. It also falls back to the babashka repl if no repls
  ;; are connected/linked, so we can always eval.
  (defadvice! corgi/around-cider-repls (command &optional type ensure)
    :around #'cider-repls
    (let ((type (cond
                 ((listp type)
                  (mapcar #'cider-maybe-intern type))
                 ((cider-maybe-intern type))))
          (repls (delete-dups (seq-mapcat #'cdr (or (sesman-current-sessions 'CIDER)
                                                    (when ensure
                                                      (user-error "No linked %s sessions" system)))))))
      (or (seq-filter (lambda (b)
                        (and (cider--match-repl-type type b)
                             (not (equal b (get-buffer "*babashka-repl*")))))
                      repls)
          (list (get-buffer "*babashka-repl*")))))

  (defun corgi/cider-eval-last-sexp-and-replace ()
    "Alternative to cider-eval-last-sexp-and-replace, but kills
clojure logical sexp instead of ELisp sexp, and pprints the
result."
    (interactive)
    (let ((last-sexp (cider-last-sexp)))
      ;; we have to be sure the evaluation won't result in an error
      (cider-nrepl-sync-request:eval last-sexp)
      ;; seems like the sexp is valid, so we can safely kill it
      (let ((opoint (point)))
        (clojure-backward-logical-sexp)
        (kill-region (point) opoint))
      (cider-interactive-eval last-sexp
                              (cider-eval-pprint-with-multiline-comment-handler
                               (current-buffer)
                               (set-marker (make-marker) (point))
                               ""
                               " "
                               "")
                              nil
                              (cider--nrepl-print-request-map fill-column))))

  (defun corgi/cider-pprint-eval-last-sexp-insert ()
    (interactive)
    (let ((cider-comment-prefix "")
          (cider-comment-continued-prefix " ")
          (cider-comment-postfix ""))
      (cider-pprint-eval-last-sexp-to-comment)))

  (defun corgi/cider-pprint-register (register)
    "Evaluate a Clojure snippet stored in a register.
Will ask for the register when used interactively. Put `#_clj' or
`#_cljs' at the start of the snippet to force evaluation to go to
a specific REPL type, no matter the mode (clojure-mode or
clojurescript-mode) of the current buffer."
    (interactive (list (register-read-with-preview "Eval register: ")))
    (let ((reg (get-register register)))
      (cond
       ((string-match-p "^#_cljs" reg)
        (with-current-buffer (car (cider-repls 'cljs))
          (cider--pprint-eval-form reg)))
       ((string-match-p "^#_clj" reg)
        (with-current-buffer (car (cider-repls 'clj))
          (cider--pprint-eval-form reg)))
       (t
        (cider--pprint-eval-form reg)))))
  (add-hook! 'cider--debug-mode-hook 'my-cider-debug-toggle-insert-state)
  (define-key cider-repl-mode-map (kbd "s-k") 'cider-quit)

  ;; cider-repl-mode only
  (define-key cider-repl-mode-map (kbd "C-c C-l") 'cider-repl-clear-buffer)
  (evil-define-key 'normal cider-repl-mode-map ",," 'cider-repl-handle-shortcut)
  (evil-define-key 'insert cider-repl-mode-map (kbd "RET") 'cider-repl-closing-return)
  (evil-define-key 'insert cider-repl-mode-map (kbd "C-n") 'cider-repl-next-input)
  (evil-define-key 'insert cider-repl-mode-map (kbd "C-p") 'cider-repl-previous-input)
  (evil-define-key 'insert cider-repl-mode-map (kbd "<C-return>") 'newline-and-indent)

  ;; add support for golden-ratio
  (with-eval-after-load 'golden-ratio
    (push 'cider-popup-buffer-quit-function golden-ratio-extra-commands))
  ;; add support for evil
  (evil-set-initial-state 'cider-stacktrace-mode 'motion)
  (evil-set-initial-state 'cider-popup-buffer-mode 'motion)
  (add-hook 'cider--debug-mode-hook 'spacemacs/cider-debug-setup)

  (evilified-state-evilify cider-browse-ns-mode cider-browse-ns-mode-map
    (kbd "d") 'cider-browse-ns-doc-at-point
    (kbd "RET") 'cider-browse-ns-operate-at-point
    (kbd "u") 'cider-browse-ns-all
    (kbd "s") 'cider-browse-ns-find-at-point
    (kbd "q") 'cider-popup-buffer-quit-function)

  (evilified-state-evilify cider-stacktrace-mode cider-stacktrace-mode-map
    (kbd "C-j") 'cider-stacktrace-next-cause
    (kbd "C-k") 'cider-stacktrace-previous-cause
    (kbd "TAB") 'cider-stacktrace-cycle-current-cause
    (kbd "0") 'cider-stacktrace-cycle-all-causes
    (kbd "1") 'cider-stacktrace-cycle-cause-1
    (kbd "2") 'cider-stacktrace-cycle-cause-2
    (kbd "3") 'cider-stacktrace-cycle-cause-3
    (kbd "4") 'cider-stacktrace-cycle-cause-4
    (kbd "5") 'cider-stacktrace-cycle-cause-5
    (kbd "a") 'cider-stacktrace-toggle-all
    (kbd "c") 'cider-stacktrace-toggle-clj
    (kbd "d") 'cider-stacktrace-toggle-duplicates
    (kbd "J") 'cider-stacktrace-toggle-java
    (kbd "r") 'cider-stacktrace-toggle-repl
    (kbd "T") 'cider-stacktrace-toggle-tooling)

  ;; open cider-doc directly and close it with q
  (setq cider-prompt-for-symbol nil)

  (evilified-state-evilify cider-docview-mode cider-docview-mode-map
    (kbd "q") 'cider-popup-buffer-quit
    (kbd "dj") 'cider-docview-javadoc
    (kbd "dw") 'cider-docview-grimoire-web
    (kbd "dg") 'cider-docview-grimoire
    (kbd "ds") 'cider-docview-source)

  (add-hook 'cider-inspector-mode-hook 'visual-line-mode)

  (evilified-state-evilify cider-inspector-mode cider-inspector-mode-map
    (kbd "b") 'evil-backward-word-begin
    (kbd "e") 'evil-forward-word-end
    (kbd "w") 'evil-forward-word-begin
    (kbd "S") 'cider-inspector-set-page-size
    (kbd "d") 'cider-inspector-def-current-val
    (kbd "n") 'cider-inspector-next-inspectable-object
    (kbd "N") 'cider-inspector-previous-inspectable-object
    (kbd "p") 'cider-inspector-previous-inspectable-object
    (kbd "M") 'evilmi-jump-items
    (kbd "B") 'cider-inspector-pop
    (kbd "C-o") 'cider-inspector-pop
    (kbd "o") 'ace-link-cider-inspector
    (kbd "J") 'cider-inspector-next-page
    (kbd "K") 'cider-inspector-prev-page
    (kbd "r") 'cider-inspector-refresh)

  (evilified-state-evilify cider-test-report-mode cider-test-report-mode-map
    (kbd "M") 'evilmi-jump-items
    (kbd "C-j") 'cider-test-next-result
    (kbd "C-k") 'cider-test-previous-result
    (kbd "RET") 'cider-test-jump
    (kbd "d") 'cider-test-ediff
    (kbd "e") 'cider-test-stacktrace
    (kbd "q") 'cider-popup-buffer-quit
    (kbd "r") 'cider-test-rerun-tests
    (kbd "t") 'cider-test-run-test
    (kbd "T") 'cider-test-run-ns-tests)

  (evil-define-key* 'normal cider-repl-mode-map
    (kbd "C-j") 'cider-repl-next-input
    (kbd "C-k") 'cider-repl-previous-input)

  (when clojure-enable-fancify-symbols
    (clojure/fancify-symbols 'cider-repl-mode)
    (clojure/fancify-symbols 'cider-clojure-interaction-mode))

  (defadvice cider-jump-to-var (before add-evil-jump activate) (evil-set-jump))

  :config/el-patch
  ;; fix cljs company-capf args out of range error
  (defun cider-completion--parse-candidate-map (candidate-map)
    "Get \"candidate\" from CANDIDATE-MAP.
Put type and ns properties on the candidate"
    (let ((candidate (nrepl-dict-get candidate-map "candidate"))
          (type (nrepl-dict-get candidate-map "type"))
          (ns (nrepl-dict-get candidate-map "ns")))
      (el-patch-swap
        (put-text-property 0 1 'type type candidate)
        (when (not (= (length candidate) 0))
          (put-text-property 0 1 'type type candidate)))
      (el-patch-swap
        (put-text-property 0 1 'ns ns candidate)
        (when (not (= (length candidate) 0))
          (put-text-property 0 1 'ns ns candidate)))
      candidate)))

(use-package clj-refactor
  :straight t
  :diminish clj-refactor-mode
  :after (clojure-mode)
  :hook ((clojurex-mode-hook
          clojurescript-mode-hook
          clojurec-mode-hook
          clojure-mode-hook)
         . clj-refactor-mode)
  :custom
  (cljr-hotload-dependencies t)
  :init
  (evil-define-key 'normal clojure-mode-map (kbd ", ESC") 'hydra-cljr-help-menu/body)
  :config
  ;; lispy-clojure file
  (defadvice! +cljr--ensure-no-dashes-in-filename (orig-fn &rest args)
    :around #'cljr--ensure-no-dashes-in-filename
    (when (not (string-match-p "lispy-clojure" (buffer-file-name)))
              (apply orig-fn args)))
  (setq cljr-cljc-clojure-test-declaration "[clojure.test :refer [deftest testing is are use-fixtures run-tests join-fixtures]]"
        cljr-cljs-clojure-test-declaration "[clojure.test :refer [deftest testing is are use-fixtures run-tests join-fixtures]]"
        cljr-clojure-test-declaration "[clojure.test :refer [deftest testing is are use-fixtures run-tests join-fixtures]]"
        cljr-eagerly-build-asts-on-startup nil
        cljr-warn-on-eval nil)
  (defhydra hydra-cljr-ns-menu (:color pink :hint nil :exit t)
    "
 Ns related refactorings
------------------------------------------------------------------------------------------------------------------------------------------------------
_ai_: Add import to ns                             _am_: Add missing libspec                          _ap_: Add project dependency
_ar_: Add require to ns                            _au_: Add use to ns                                _cn_: Clean ns
_rm_: Require a macro into the ns                  _sr_: Stop referring
_b_: Back to previous Hydra
"
    ("ai" cljr-add-import-to-ns) ("am" cljr-add-missing-libspec)
    ("ap" cljr-add-project-dependency) ("ar" cljr-add-require-to-ns)
    ("au" cljr-add-use-to-ns) ("cn" cljr-clean-ns)
    ("rm" cljr-require-macro) ("sr" cljr-stop-referring)
    ("b" hydra-cljr-help-menu/body :exit t)
    ("q" nil "quit"))
  (defhydra hydra-cljr-code-menu (:color pink :hint nil :exit t)
    "
 Code related refactorings
------------------------------------------------------------------------------------------------------------------------------------------------------
_ci_: Cycle if                                     _ct_: Cycle thread
_dk_: Destructure keys                             _el_: Expand let                                   _fu_: Find usages
_il_: Introduce let                                _is_: Inline symbol                                _ml_: Move to let
_pf_: Promote function                             _rl_: Remove let                                   _rs_: Rename symbol
_tf_: Thread first all                             _th_: Thread                                       _tl_: Thread last all
_ua_: Unwind all                                   _uw_: Unwind
_b_: Back to previous Hydra
"
    ("ci" clojure-cycle-if) ("ct" cljr-cycle-thread)
    ("dk" cljr-destructure-keys) ("el" cljr-expand-let)
    ("fu" cljr-find-usages) ("il" cljr-introduce-let)
    ("is" cljr-inline-symbol) ("ml" cljr-move-to-let)
    ("pf" cljr-promote-function) ("rl" cljr-remove-let)
    ("rs" cljr-rename-symbol) ("tf" clojure-thread-first-all)
    ("th" clojure-thread) ("tl" clojure-thread-last-all)
    ("ua" clojure-unwind-all) ("uw" clojure-unwind)
    ("b" hydra-cljr-help-menu/body :exit t)
    ("q" nil "quit"))
  (defhydra hydra-cljr-project-menu (:color pink :hint nil :exit t)
    "
 Project related refactorings
------------------------------------------------------------------------------------------------------------------------------------------------------
_ap_: Add project dependency                       _cs_: Change function signature                    _fu_: Find usages
_hd_: Hotload dependency                           _is_: Inline symbol                                _mf_: Move form
_pc_: Project clean                                _rf_: Rename file-or-dir _rs_: Rename symbol       _sp_: Sort project dependencies
_up_: Update project dependencies
_b_: Back to previous Hydra
"
    ("ap" cljr-add-project-dependency) ("cs" cljr-change-function-signature)
    ("fu" cljr-find-usages) ("hd" cljr-hotload-dependency)
    ("is" cljr-inline-symbol) ("mf" cljr-move-form)
    ("pc" cljr-project-clean) ("rf" cljr-rename-file-or-dir)
    ("rs" cljr-rename-symbol) ("sp" cljr-sort-project-dependencies)
    ("up" cljr-update-project-dependencies)
    ("b" hydra-cljr-help-menu/body :exit t)
    ("q" nil "quit"))
  (defhydra hydra-cljr-toplevel-form-menu (:color pink :hint nil :exit t)
    "
 Toplevel form related refactorings
------------------------------------------------------------------------------------------------------------------------------------------------------
_as_: Add stubs for the interface/protocol at point_cp_: Cycle privacy                                _cs_: Change function signature
_ec_: Extract constant                             _ed_: Extract form as def                          _ef_: Extract function
_fe_: Create function from example                 _is_: Inline symbol                                _mf_: Move form
_pf_: Promote function                             _rf_: Rename file-or-dir                           _ad_: Add declaration
_b_: Back to previous Hydra
"
    ("as" cljr-add-stubs) ("cp" clojure-cycle-privacy)
    ("cs" cljr-change-function-signature) ("ec" cljr-extract-constant)
    ("ed" cljr-extract-def) ("ef" cljr-extract-function)
    ("fe" cljr-create-fn-from-example) ("is" cljr-inline-symbol)
    ("mf" cljr-move-form) ("pf" cljr-promote-function)
    ("rf" cljr-rename-file-or-dir) ("ad" cljr-add-declaration)
    ("b" hydra-cljr-help-menu/body :exit t)
    ("q" nil "quit"))

  (cljr-add-keybindings-with-prefix "C-c j")

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

(use-package clj-ns-name
  :straight (:host github :repo "plexus/plexmacs" :files ("clj-ns-name/clj-ns-name.el"))
  :config
  (clj-ns-name-install))

(use-package pprint-to-buffer
  :straight (:type git
                   :host github
                   :files ("pprint-to-buffer/pprint-to-buffer.el")
                   :repo "plexus/plexmacs")
  :after (cider)
  :commands (pprint-to-buffer-last-sexp-to-current-buffer pprint-to-buffer-last-sexp))

(use-package walkclj
 :straight (:type git :host github :files ("walkclj.el") :repo "plexus/walkclj")
 :disabled)

(use-package cljr-ivy
  :straight t
  :bind (:map clojure-mode-map) ("C-c r" . cljr-ivy))

(use-package cider-eval-sexp-fu
  :straight t
  :after cider)

(use-package clojure-snippets
  :straight t
  :after clojure-mode)

;; (use-package flycheck-clojure
;;   :straight t
;;   :after (flycheck cider)
;;   :hook (clojure-mode . flycheck-clojure-setup))

(use-package clojure-mode-extra-font-locking
  :straight t
  :disabled
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

(use-package sayid
  :straight t
  :disabled
  :commands (sayid-setup-package)
  :after (clojure-mode cider)
  :init
  (defun yq/jump-to-sayid-buffer ()
    (interactive)
    (if (get-buffer-window "*sayid*")
        (pop-to-buffer "*sayid*")))

  (defun yq/jump-to-sayid-trace-buffer ()
    (interactive)
    (if (get-buffer-window "*sayid-traced*")
        (pop-to-buffer "*sayid-traced*")))

  (advice-add #'sayid-get-workspace :after 'yq/jump-to-sayid-buffer)
  (advice-add #'sayid-show-traced :after 'yq/jump-to-sayid-trace-buffer)
  (advice-add #'sayid-show-traced-ns :after 'yq/jump-to-sayid-trace-buffer)

  (defun yq/sayid-trace-fn-load-enable-clear(arg)
    (interactive "P")
    (cider-ns-refresh 'refresh-all)
    (sayid-trace-fn-enable)
    (sayid-load-enable-clear)
    (if arg (yq/jump-to-sayid-buffer)))

  (defun yq/sayid-trace-file-load-enable-clear(arg)
    (interactive "P")
    (cider-ns-refresh 'refresh-all)
    (sayid-trace-ns-in-file)
    (sayid-load-enable-clear)
    (if arg (yq/jump-to-sayid-buffer)))

  (add-hook 'clojure-mode-hook 'sayid-setup-package)
  (dolist (map (list clojure-mode-map
                     clojurec-mode-map
                     clojurescript-mode-map
                     cider-repl-mode-map))
    (evil-define-key* 'normal map
                      ;;These keybindings mostly preserved from the default sayid bindings
                      ;; ",d!" 'sayid-load-enable-clear
                      ",," 'yq/sayid-trace-file-load-enable-clear
                      ", " 'yq/sayid-trace-fn-load-enable-clear
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
                      ",dtU" 'sayid-kill-all-traces
                      ",di" 'sayid-inner-trace-fn
                      ",dI" 'sayid-outer-trace-fn
                      ",dtp" 'sayid-trace-ns-by-pattern
                      ",dtr" 'sayid-remove-trace-fn
                      ",dT" 'sayid-trace-all-ns-in-dir
                      ",dV" 'sayid-set-view
                      ",dw" 'sayid-get-workspace
                      ",dx" 'sayid-reset-workspace))
  :config
  (evilified-state-evilify sayid-mode sayid-mode-map
    (kbd "H") 'sayid-buf-forward
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
    (kbd "h") 'sayid-traced-buf-show-help))

(use-package flycheck-clj-kondo
  :straight t
  :hook (clojure-mode . (lambda () (require 'flycheck-clj-kondo))))

(use-package 4clojure
  :straight (:host github :repo "yqrashawn/4clojure.el")
  :defer t)

(use-package hugsql-ghosts
  :straight (:host github :repo "rkaercher/hugsql-ghosts")
  :after cider
  :hook (cider-mode . hugsql-ghosts-install-hook))

(use-package clojure-essential-ref
  :straight (:host github :repo "p3r7/clojure-essential-ref")
  :commands (clojure-essential-ref))

(provide 'clojure)
;;; clojure.el ends here
