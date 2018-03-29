(use-package web-mode
  :straight t
  :mode
  (("\\.phtml\\'"      . web-mode)
   ("\\.tpl\\.php\\'"  . web-mode)
   ("\\.twig\\'"       . web-mode)
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
  :init
  (progn
    (spacemacs|add-company-backends
      :backends (company-web-html company-css)
      :modes web-mode
      :variables
      ;; see https://github.com/osv/company-web/issues/4
      company-minimum-prefix-length 0)))