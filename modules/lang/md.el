(use-package markdown-mode
  :mode
  (("\\.m[k]d" . markdown-mode)
   ("\\.mdk" . markdown-mode))
  :defer t
  :config
  (evil-define-key 'normal markdown-mode-map
    ;; Movement
    ",{"   'markdown-backward-paragraph
    ",}"   'markdown-forward-paragraph
    ;; Completion, and Cycling
    ",]"   'markdown-complete
    ;; Indentation
    ",>"   'markdown-indent-region
    ",<"   'markdown-exdent-region
    ;; Buffer-wide commands
    ",c]"  'markdown-complete-buffer
    ",cc"  'markdown-check-refs
    ",ce"  'markdown-export
    ",cm"  'markdown-other-window
    ",cn"  'markdown-cleanup-list-numbers
    ",co"  'markdown-open
    ",cp"  'markdown-preview
    ",cv"  'markdown-export-and-preview
    ",cw"  'markdown-kill-ring-save
    ;; headings
    ",hi"  'markdown-insert-header-dwim
    ",hI"  'markdown-insert-header-setext-dwim
    ",h1"  'markdown-insert-header-atx-1
    ",h2"  'markdown-insert-header-atx-2
    ",h3"  'markdown-insert-header-atx-3
    ",h4"  'markdown-insert-header-atx-4
    ",h5"  'markdown-insert-header-atx-5
    ",h6"  'markdown-insert-header-atx-6
    ",h!"  'markdown-insert-header-setext-1
    ",h@"  'markdown-insert-header-setext-2
    ;; Insertion of common elements
    ",-"   'markdown-insert-hr
    ",if"  'markdown-insert-footnote
    ",ii"  'markdown-insert-image
    ",ik"  'spacemacs/insert-keybinding-markdown
    ",iI"  'markdown-insert-reference-image
    ",il"  'markdown-insert-inline-link-dwim
    ",iL"  'markdown-insert-reference-link-dwim
    ",iw"  'markdown-insert-wiki-link
    ",iu"  'markdown-insert-uri
    ;; Element removal
    ",k"   'markdown-kill-thing-at-point
    ;; List editing
    ",li"  'markdown-insert-list-item
    ;; Toggles
    ",ti"  'markdown-toggle-inline-images
    ",tl"  'markdown-toggle-url-hiding
    ",tt"  'markdown-toggle-gfm-checkbox
    ",tw"  'markdown-toggle-wiki-links
    ;; region manipulation
    ",xb"  'markdown-insert-bold
    ",xi"  'markdown-insert-italic
    ",xc"  'markdown-insert-code
    ",xC"  'markdown-insert-gfm-code-block
    ",xq"  'markdown-insert-blockquote
    ",xQ"  'markdown-blockquote-region
    ",xp"  'markdown-insert-pre
    ",xP"  'markdown-pre-region
    ;; Following and Jumping
    ",N"   'markdown-next-link
    ",f"   'markdown-follow-thing-at-point
    ",P"   'markdown-previous-link
    "<RET>" 'markdown-jump)
  ;; Header navigation in normal state movements
  (evil-define-key 'normal markdown-mode-map
    "gj" 'outline-forward-same-level
    "gk" 'outline-backward-same-level
    "gh" 'outline-up-heading
    ;; next visible heading is not exactly what we want but close enough
    "gl" 'outline-next-visible-heading)
  ;; Promotion, Demotion
  (define-key markdown-mode-map (kbd "M-h") 'markdown-promote)
  (define-key markdown-mode-map (kbd "M-j") 'markdown-move-down)
  (define-key markdown-mode-map (kbd "M-k") 'markdown-move-up)
  (define-key markdown-mode-map (kbd "M-l") 'markdown-demote))

(use-package vmd-mode
  :straight t
  :defer t
  :init
  (evil-define-key 'normal markdown-mode-map
    ",cP" 'vmd-mode))