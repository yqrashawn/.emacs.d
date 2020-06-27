;;; c.el --- configs language c -*- lexical-binding: t; -*-

(yq/get-modules "lang/c-funcs.el")
(defconst c-c++-modes '(c-mode c++-mode)
  "Primary major modes of the `c-c++' layer.")

(defconst c-c++-mode-hooks '(c-mode-hook c++-mode-hook)
  "Primary hooks of the `c-c++' layer.")

(defvar c-c++-enable-clang-support t
  "If non nil Clang related packages and configuration are enabled.")

(defvar c-c++-enable-google-style nil
  "If non-nil `google-set-c-style' will be added as as
  `c-mode-common-hook'.")

(defvar c-c++-enable-google-newline nil
  "If non-nil `google-make-newline-indent' will be added as as
  `c-mode-common-hook'.")

(defvar c-c++-enable-rtags-support nil
  "If non nil Rtags related packages and configuration are enabled.")

(defvar c-c++-enable-cmake-ide-support t
  "If non nil CMake related packages and configuration are enabled.")

(defvar c-c++-enable-clang-format-on-save t
  "If non-nil, automatically format code with ClangFormat on
  save. Clang support has to be enabled for this to work.")

(defvar c-c++-enable-c++11 t
  "If non nil then c++11 related features will be enabled")

;; (spacemacs|define-jump-handlers c++-mode)
;; (spacemacs|define-jump-handlers c-mode)

(defvar c-c++-default-mode-for-headers 'c-mode
  "Default mode to open header files. Can be `c-mode' or `c++-mode'.")

(use-package cc-mode
  :straight t
  :defer t
  :init
  (add-to-list 'auto-mode-alist
               `("\\.h\\'" . ,c-c++-default-mode-for-headers))
  (spacemacs/add-to-hooks 'semantic-mode c-c++-mode-hooks)
  :config
  (require 'compile)
  (c-toggle-auto-newline 1)
  (dolist (mode c-c++-modes)
    (evil-define-key 'normal mode
      ",ga" 'projectile-find-other-file
      ",gA" 'projectile-find-other-file-other-window))
  (spacemacs|add-company-backends :backends (company-lsp company-cmake) :modes cmake-mode :after-hook t)
  (when c-c++-enable-clang-support
    (spacemacs|add-company-backends :backends (company-lsp company-clang)
                                    :modes c-mode-common
                                    :after-hook t)
    (when c-c++-enable-c++11
      (setq company-clang-arguments '("-std=c++11")))
    (setq company-clang-prefix-guesser 'spacemacs/company-more-than-prefix-guesser)
    (spacemacs/add-to-hooks 'spacemacs/c-c++-load-clang-args c-c++-mode-hooks))
  (dolist (mode c-c++-modes)
    (spacemacs/enable-flycheck mode))
  (when c-c++-enable-clang-support
    (spacemacs/add-to-hooks 'spacemacs/c-c++-load-clang-args c-c++-mode-hooks)
    (when c-c++-enable-c++11
      (setq flycheck-clang-language-standard "c++11"))))

(use-package company-c-headers
  :straight t
  :defer t
  :init (spacemacs|add-company-backends
          :backends (company-lsp company-c-headers)
          :modes c-mode-common
          :after-hook t))

(use-package clang-format
  :straight t
  :if c-c++-enable-clang-support
  :init
  (when c-c++-enable-clang-format-on-save
    (spacemacs/add-to-hooks 'spacemacs/clang-format-on-save c-c++-mode-hooks))
  (dolist (mode c-c++-modes)
    (evil-define-key 'normal mode
      ",==" 'spacemacs/clang-format-region-or-buffer
      ",=f" 'spacemacs/clang-format-function)))

;; (use-package realgud
;;   :straight t
;;   :defer t
;;   :commands (realgud:gdb)
;;   :init
;;   (dolist (mode c-c++-modes)
;;     (evil-define-key 'normal mode
;;       "dd" 'realgud:gdb
;;       "de" 'realgud:cmd-eval-dwim))
;;   (defun spacemacs//short-key-state (modeon)
;;     "Set evil-evilified-state explicitly."
;;     (if modeon
;;         (evil-evilified-state)
;;       (evil-normal-state)))
;;   (advice-add 'realgud-short-key-mode-setup
;;               :before #'spacemacs//short-key-state)
;;   (evilified-state-evilify-map realgud:shortkey-mode-map
;;     :eval-after-load realgud
;;     :mode realgud-short-key-mode
;;     :bindings
;;     "s" 'realgud:cmd-next
;;     "i" 'realgud:cmd-step
;;     "b" 'realgud:cmd-break
;;     "B" 'realgud:cmd-clear
;;     "o" 'realgud:cmd-finish
;;     "c" 'realgud:cmd-continue
;;     "e" 'realgud:cmd-eval
;;     "r" 'realgud:cmd-restart
;;     "q" 'realgud:cmd-quit
;;     "S" 'realgud-window-cmd-undisturb-src))

;; (use-package cmake-ide
;;   :straight t
;;   :if c-c++-enable-cmake-ide-support
;;   :config
;;   (cmake-ide-setup)
;;   (evil-define-key 'normal c++-mode
;;     ",cc" 'cmake-ide-compile
;;     ",pc" 'cmake-ide-run-cmake
;;     ",pC" 'cmake-ide-maybe-run-cmake
;;     ",pd" 'cmake-ide-delete-file)
;;   (dolist (mode c-c++-modes)
;;     (evil-define-key 'normal mode
;;       ",cc" 'cmake-ide-compile
;;       ",pc" 'cmake-ide-run-cmake
;;       ",pC" 'cmake-ide-maybe-run-cmake
;;       ",pd" 'cmake-ide-delete-file)))

(use-package cmake-mode
  :straight t
  :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode)))