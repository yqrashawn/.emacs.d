(yq/get-modules "lang/js2-imenu.el")
(spacemacs|define-jump-handlers js2-mode)
(setq js-indent-level 2)
(use-package js2-mode
  :straight t
  :mode "\\.js\\'"
  :diminish (js2-mode . "JS")
  :init
  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
  :config
  (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS")))
  ;; @see https://github.com/mooz/js2-mode/issues/350
  (setq forward-sexp-function nil)
  (setq js2-mode-show-parse-errors t)
  (setq js2-mode-show-strict-warnings nil)
  (evil-define-key 'normal js2-mode-map "," nil)
  (evil-define-key 'normal js2-mode-map ",d" nil)
  (evil-define-key 'normal js2-mode-map ",zc" 'js2-mode-hide-element)
  (evil-define-key 'normal js2-mode-map ",zo" 'js2-mode-show-element)
  (evil-define-key 'normal js2-mode-map ",zr" 'js2-mode-show-all)
  (evil-define-key 'normal js2-mode-map ",ze" 'js2-mode-toggle-element)
  (evil-define-key 'normal js2-mode-map ",zf" 'js2-mode-toggle-hide-functions)
  (evil-define-key 'normal js2-mode-map ",zC" 'js2-mode-toggle-hide-comments)
  (evil-define-key 'normal js2-mode-map ",w" 'js2-mode-toggle-warnings-and-errors))

(use-package js-doc
  :straight t
  :commands (js-doc-insert-file-doc js-doc-insert-function-doc js-doc-insert-tag js-doc-describe-tag):init
  (evil-define-key 'normal js2-mode-map ",db" 'js-doc-insert-file-doc)
  (evil-define-key 'normal js2-mode-map ",df" 'js-doc-insert-function-doc)
  (evil-define-key 'normal js2-mode-map ",df" 'js-doc-insert-tag)
  (evil-define-key 'normal js2-mode-map ",df" 'js-doc-describe-tag))

(defun spacemacs//tern-detect ()
  "Detect tern binary and warn if not found."
  (let ((found (executable-find "tern")))
    (unless found
      (message "tern binary not found!"))
    found))

(defun yq//set-tern-key-bindings (mode)
  "Set the key bindings for tern and the given MODE."
  (add-to-list 'tern-command "--no-port-file" 'append)
  (add-to-list (intern (format "spacemacs-jump-handlers-%S" mode))
               '(tern-find-definition :async t) t)
  (evil-define-key 'normal js2-mode-map ",t" nil)
  (evil-define-key 'normal js2-mode-map ",tf" 'tern-find-definition)
  (evil-define-key 'normal js2-mode-map ",tr" 'tern-rename-variable)
  (evil-define-key 'normal js2-mode-map ",td" 'tern-get-docs)
  (evil-define-key 'normal js2-mode-map ",tn" 'tern-find-definition-by-name)
  (evil-define-key 'normal js2-mode-map ",tp" 'tern-pop-find-definition)
  (evil-define-key 'normal js2-mode-map ",tt" 'tern-get-type))

(use-package tern
  :defer t
  :commands (tern-mode)
  :diminish tern-mode
  :hook (js2-mode . tern-mode)
  :init
  (spacemacs//tern-detect)
  :config
  (add-to-list 'tern-command "--no-port-file" 'append)
  (yq//set-tern-key-bindings 'js2-mode)
  (dolist (mode '(js2-mode json-mode))
    (spacemacs/enable-flycheck mode)))

(use-package company-tern
  :straight t
  :after js2-mode
  :init
  (spacemacs|add-company-backends
    :backends company-tern
    :modes js2-mode
    :after-hook t))

(use-package json-mode
  :straight t
  :init
  :mode(("\\.json\\'" . json-mode)
        ("\\manifest.webapp\\'" . json-mode)
        ("\\.eslintrc\\'" . json-mode)
        ("\\.tern-project\\'" . json-mode)))

;; (bound-and-true-p prettier-js-mode)
(use-package prettier-js
  :straight t
  :diminish prettier-js-mode
  :commands (prettier-js-mode prettier-js)
  :hook (js2-mode . prettier-js-mode)
  :hook (rjsx-mode . prettier-js-mode)
  :hook (typescript-mode . prettier-js-mode)
  :config
  (yq/add-toggle prettier-js :mode prettier-js-mode)
  (evil-define-key 'normal js2-mode-map ",=" 'prettier-js)
  (evil-define-key 'normal js2-mode-map ",tp" 'yq/toggle-prettier-js))

(use-package rjsx-mode
  :straight t
  :defer t
  :mode (("\\.jsx\\'" . rjsx-mode) ("components\\/.*\\.js\\'" . rjsx-mode))
  :commands (rjsx-delete-creates-full-tag rjsx-electric-gt rjsx-electric-lt rjsx-rename-tag-at-point)
  :init
  :config
  (defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
    "Workaround sgml-mode and follow airbnb component style."
    (save-excursion
      (beginning-of-line)
      (if (looking-at-p "^ +\/?> *$")
          (delete-char sgml-basic-offset))))
  (evil-define-key 'insert rjsx-mode-map (kbd "C-d") 'rjsx-delete-creates-full-tag
    (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode "=" 'prettier-js)
    (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode "m" 'js2-mode)))

(use-package js2-refactor
  :straight t
  :after js2-mode
  :hook (js2-mode . js2-refactor-mode)
  :commands (js2r-inline-var
             js2r-rename-var
             js2r-var-to-this
             js2r-ternary-to-if
             js2r-log-this
             js2r-kill
             js2r-toggle-function-async
             js2r-expand-node-at-point
             js2r--expand-contract-node-at-point)
  :init
  (evil-define-key 'normal js2-mode-map ",iv" 'js2r-inline-var)
  (evil-define-key 'normal js2-mode-map ",rv" 'js2r-rename-var)
  (evil-define-key 'normal js2-mode-map ",vt" 'js2r-var-to-this)
  (evil-define-key 'normal js2-mode-map ",3i" 'js2r-ternary-to-if)
  (evil-define-key 'normal js2-mode-map ",c" 'js2r-log-this)
  (evil-define-key 'normal js2-mode-map ",k" 'js2r-kill)
  (evil-define-key 'normal js2-mode-map ",ta" 'js2r-toggle-function-async)
  (evil-define-key 'normal js2-mode-map ",ep" 'js2r-expand-node-at-point)
  (evil-define-key 'normal js2-mode-map ",ec" 'js2r--expand-contract-node-at-point)
  (evil-define-key 'normal js2-mode-map ",," 'js2-refactor-hydra/body)
  (defhydra js2-refactor-hydra (:color blue :hint nil)
    "
^Functions^                    ^Variables^               ^Buffer^                      ^sexp^               ^Debugging^
------------------------------------------------------------------------------------------------------------------------------
[_lp_] Localize Parameter      [_ev_] Extract variable   [_wi_] Wrap buffer in IIFE    [_k_]  js2 kill      [_lt_] log this
[_ef_] Extract function        [_iv_] Inline variable    [_ig_] Inject global in IIFE  [_ss_] split string  [_dt_] debug this
[_ip_] Introduce parameter     [_rv_] Rename variable    [_ee_] Expand node at point   [_sl_] forward slurp
[_em_] Extract method          [_vt_] Var to this        [_cc_] Contract node at point [_ba_] forward barf
[_ao_] Arguments to object     [_sv_] Split var decl.    [_uw_] unwrap
[_tf_] Toggle fun exp and decl [_ag_] Add var to globals
[_ta_] Toggle fun expr and =>  [_ti_] Ternary to if
[_q_]  quit"
    ("ee" js2r-expand-node-at-point)
    ("cc" js2r-contract-node-at-point)
    ("ef" js2r-extract-function)
    ("em" js2r-extract-method)
    ("tf" js2r-toggle-function-expression-and-declaration)
    ("ta" js2r-toggle-arrow-function-and-expression)
    ("ip" js2r-introduce-parameter)
    ("lp" js2r-localize-parameter)
    ("wi" js2r-wrap-buffer-in-iife)
    ("ig" js2r-inject-global-in-iife)
    ("ag" js2r-add-to-globals-annotation)
    ("ev" js2r-extract-var)
    ("iv" js2r-inline-var)
    ("rv" js2r-rename-var)
    ("vt" js2r-var-to-this)
    ("ao" js2r-arguments-to-object)
    ("ti" js2r-ternary-to-if)
    ("sv" js2r-split-var-declaration)
    ("ss" js2r-split-string)
    ("uw" js2r-unwrap)
    ("lt" js2r-log-this)
    ("dt" js2r-debug-this)
    ("sl" js2r-forward-slurp)
    ("ba" js2r-forward-barf)
    ("k" js2r-kill)
    ("q" nil)
    (",," nil)))

(use-package add-node-modules-path
  :straight t
  :after js2-mode
  :hook (js2-mode . #'add-node-modules-path))