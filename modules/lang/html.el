;;; html.el ---  html packages -*- lexical-binding: t; -*-

(use-package web-mode
  :straight t
  :mode
  (("\\.phtml\\'"      . web-mode)
   ("\\.tpl\\.php\\'"  . web-mode)
   ("\\.twig\\'"       . web-mode)
   ("\\.vue\\'"        . web-mode)
   ("\\.xml\\'"        . web-mode)
   ("\\.html\\'"       . web-mode)
   ("\\.htm\\'"        . web-mode)
   ("\\.[gj]sp\\'"     . web-mode)
   ("\\.as[cp]x?\\'"   . web-mode)
   ("\\.eex\\'"        . web-mode)
   ("\\.erb\\'"        . web-mode)
   ("\\.mustache\\'"   . web-mode)
   ("\\.handlebars\\'" . web-mode)
   ("\\.hbs\\'"        . web-mode)
   ("\\.eco\\'"        . web-mode)
   ("\\.ejs\\'"        . web-mode)
   ("\\.djhtml\\'"     . web-mode))
  :config
  (add-hook 'web-mode-hook (lambda ()
                             (when web-mode
                               (evil-define-key nil evil-inner-text-objects-map "t" #'evil-inner-tag)
                               (evil-define-key nil evil-outer-text-objects-map "t" #'evil-a-tag))))
  ;; ugly imenu for vue functions
  ;; for better looking one https://github.com/fxbois/web-mode/issues/886
  (add-to-list 'web-mode-imenu-regexp-list
               '("^[\s\t]*\\(async\s+\\)?[a-zA-Z0-9_]+\s*([a-z-aZ0-9_,=]*)" 0 9 ""))
  (add-to-list 'web-mode-imenu-regexp-list
               '("^[\s\t]*\\(async\s+\\)?\\(function\s*\\)[a-zA-Z0-9_]+\s*([a-z-aZ0-9_,=]*)" 0 9 ""))
  (add-to-list 'web-mode-imenu-regexp-list
               '("^[\s\t]*[a-zA-Z0-9_]+\s*:\s*\\(function\\)?\s*([a-zA-Z0-9_,\s]*)\\(\s*=>\s*\\)?\s*{" 0 9 ""))
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2
        web-mode-enable-auto-expanding t
        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-column-highlight nil
        web-mode-enable-current-element-highlight nil)
  (evil-define-key 'normal web-mode-map
    (kbd "C-m") 'evilmi-jump-items
    ",El" 'web-mode-dom-errors-show
    ",gb" 'web-mode-element-beginning
    ",gc" 'web-mode-element-child
    ",gp" 'web-mode-element-parent
    ",gs" 'web-mode-element-sibling-next
    ",hp" 'web-mode-dom-xpath
    ",rc" 'web-mode-element-clone
    ",rd" 'web-mode-element-vanish
    ",rk" 'web-mode-element-kill
    ",rr" 'web-mode-element-rename
    ",rw" 'web-mode-element-wrap
    ",z" 'web-mode-fold-or-unfold))

(use-package company-web
  :straight t
  :defer t
  :after web-mode
  :init
  (spacemacs|add-company-backends
    :backends (company-lsp company-web-html company-css company-web-jade)
    :modes web-mode
    :variables
    ;; see https://github.com/osv/company-web/issues/4
    company-minimum-prefix-length 0
    :after-hook t))

(use-package auto-rename-tag
  :straight t
  :hook (web-mode . auto-rename-tag-mode))
