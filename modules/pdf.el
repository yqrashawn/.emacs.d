(use-package pdf-tools
  :straight t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :diminish pdf-view-midnight-minor-mode
  :init
  (yq/update-evil-emacs-state-modes 'pdf-view-mode)
  (yq/update-evil-emacs-state-modes 'pdf-outline-buffer-mode)
  (yq/update-evil-emacs-state-modes 'pdf-annot-list-mode)
  ;; initialise
  (pdf-tools-install)
  :config
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t)
  ;; use normal isearch
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (evil-define-key 'insert pdf-view-mode-map "h" 'image-backward-hscroll)
  (evil-define-key 'insert pdf-view-mode-map "l" 'image-forward-hscroll)
  (evil-define-key 'insert pdf-view-mode-map "<" 'image-bob)
  (evil-define-key 'insert pdf-view-mode-map ">" 'image-eob)
  (evil-define-key 'insert pdf-view-mode-map "n" 'pdf-view-next-page-command)
  (evil-define-key 'insert pdf-view-mode-map "p" 'pdf-view-previous-page-command)
  (evil-define-key 'insert pdf-view-mode-map "d" 'pdf-view-next-page-command)
  (evil-define-key 'insert pdf-view-mode-map "u" 'pdf-view-previous-page-command)
  (evil-define-key 'insert pdf-view-mode-map "j" 'image-next-line)
  (evil-define-key 'insert pdf-view-mode-map "k" 'image-previous-line)
  (evil-define-key 'insert pdf-view-mode-map "gg" 'image-bob)
  (evil-define-key 'insert pdf-view-mode-map "G" 'image-eob)
  (evil-define-key 'insert pdf-view-mode-map "[[" 'pdf-view-first-page)
  (evil-define-key 'insert pdf-view-mode-map "]]" 'pdf-view-last-page)
  (evil-define-key 'insert pdf-view-mode-map "o" 'pdf-outline)
  (evil-define-key 'insert pdf-view-mode-map "\C-s" 'isearch-forward)
  (evil-define-key 'insert pdf-view-mode-map "H" 'pdf-history-backward)
  (evil-define-key 'insert pdf-view-mode-map "L" 'pdf-history-forward)
  (evil-define-key 'insert pdf-view-mode-map "f" 'pdf-links-action-perform)
  (evil-define-key 'insert pdf-view-mode-map "F" 'pdf-links-isearch-link)
  (evil-define-key 'insert pdf-view-mode-map "a" 'nil)
  (evil-define-key 'insert pdf-view-mode-map "al" 'pdf-annot-list-annotations)
  (evil-define-key 'insert pdf-view-mode-map ":" 'pdf-view-goto-page)
  (evil-define-key 'insert pdf-view-mode-map "O" 'pdf-occur)
  (evil-define-key 'insert pdf-view-mode-map "1" 'pdf-view-fit-height-to-window)
  (evil-define-key 'insert pdf-view-mode-map "2" 'pdf-view-fit-width-to-window)
  (evil-define-key 'insert pdf-view-mode-map "3" 'pdf-view-fit-page-to-window)
  (evil-define-key 'insert pdf-view-mode-map "mt" 'pdf-view-set-slice-from-bounding-box)
  (evil-define-key 'insert pdf-view-mode-map "mr" 'pdf-view-reset-slice)
  (evil-define-key 'insert pdf-view-mode-map "w" 'pdf-view-shrink)
  (evil-define-key 'insert pdf-view-mode-map "e" 'pdf-view-enlarge)
  (evil-define-key 'insert pdf-view-mode-map "Q" 'quit-window)
  (evil-define-key 'insert pdf-view-mode-map "ah" 'pdf-annot-add-highlight-markup-annotation)
  (evil-define-key 'insert pdf-view-mode-map "au" 'pdf-annot-add-underline-markup-annotation)
  (evil-define-key 'insert pdf-view-mode-map "as" 'pdf-annot-add-strikeout-markup-annotation)
  (evil-define-key 'insert pdf-view-mode-map "av" 'pdf-annot-add-squiggly-markup-annotation)
  (evil-define-key 'insert pdf-view-mode-map "am" 'pdf-annot-add-markup-annotation)
  (evil-define-key 'insert pdf-view-mode-map "4" 'pdf-view-midnight-minor-mode)
  (evil-define-key 'insert pdf-annot-list-mode-map "d" 'tablist-flag-forward)
  (evil-define-key 'insert pdf-annot-list-mode-map "u" 'tablist-unmark-forward)
  (evil-define-key 'insert pdf-annot-list-mode-map "x" 'tablist-do-flagged-delete)
  (evil-define-key 'insert pdf-annot-list-mode-map "q" 'tablist-quit)
  (evil-define-key 'insert pdf-annot-list-mode-map (kbd "RET" ) 'pdf-annot-list-display-annotation-from-id)
  (evil-define-key 'insert pdf-occur-buffer-mode-map (kbd "RET") 'pdf-occur-goto-occurrence)
  (evil-define-key 'insert pdf-outline-buffer-mode-map (kbd "RET") 'pdf-outline-display-link)
  (evil-define-key 'insert pdf-outline-buffer-mode-map "f" 'pdf-outline-follow-mode)
  (evil-define-key 'insert pdf-outline-buffer-mode-map "q" 'pdf-outline-quit)
  (evil-define-key 'insert pdf-outline-buffer-mode-map (kbd "TAB") 'pdf-outline-toggle-subtree)
  (evil-define-key 'normal pdf-outline-buffer-mode-map (kbd "TAB") 'pdf-outline-toggle-subtree)
  (evil-define-key 'normal pdf-view-mode-map "h" 'image-backward-hscroll)
  (evil-define-key 'normal pdf-view-mode-map "l" 'image-forward-hscroll)
  (evil-define-key 'normal pdf-view-mode-map "<" 'image-bob)
  (evil-define-key 'normal pdf-view-mode-map ">" 'image-eob)
  (evil-define-key 'normal pdf-view-mode-map "n" 'pdf-view-next-page-command)
  (evil-define-key 'normal pdf-view-mode-map "p" 'pdf-view-previous-page-command)
  (evil-define-key 'normal pdf-view-mode-map "d" 'pdf-view-next-page-command)
  (evil-define-key 'normal pdf-view-mode-map "u" 'pdf-view-previous-page-command)
  (evil-define-key 'normal pdf-view-mode-map "j" 'image-next-line)
  (evil-define-key 'normal pdf-view-mode-map "k" 'image-previous-line)
  (evil-define-key 'normal pdf-view-mode-map "gg" 'image-bob)
  (evil-define-key 'normal pdf-view-mode-map "G" 'image-eob)
  (evil-define-key 'normal pdf-view-mode-map "[[" 'pdf-view-first-page)
  (evil-define-key 'normal pdf-view-mode-map "]]" 'pdf-view-last-page)
  (evil-define-key 'normal pdf-view-mode-map "o" 'pdf-outline)
  (evil-define-key 'normal pdf-view-mode-map "\C-s" 'isearch-forward)
  (evil-define-key 'normal pdf-view-mode-map "H" 'pdf-history-backward)
  (evil-define-key 'normal pdf-view-mode-map "L" 'pdf-history-forward)
  (evil-define-key 'normal pdf-view-mode-map "f" 'pdf-links-action-perform)
  (evil-define-key 'normal pdf-view-mode-map "F" 'pdf-links-isearch-link)
  (evil-define-key 'normal pdf-view-mode-map "a" 'nil)
  (evil-define-key 'normal pdf-view-mode-map "al" 'pdf-annot-list-annotations)
  (evil-define-key 'normal pdf-view-mode-map ":" 'pdf-view-goto-page)
  (evil-define-key 'normal pdf-view-mode-map "O" 'pdf-occur)
  (evil-define-key 'normal pdf-view-mode-map "1" 'pdf-view-fit-height-to-window)
  (evil-define-key 'normal pdf-view-mode-map "2" 'pdf-view-fit-width-to-window)
  (evil-define-key 'normal pdf-view-mode-map "3" 'pdf-view-fit-page-to-window)
  (evil-define-key 'normal pdf-view-mode-map "mt" 'pdf-view-set-slice-from-bounding-box)
  (evil-define-key 'normal pdf-view-mode-map "mr" 'pdf-view-reset-slice)
  (evil-define-key 'normal pdf-view-mode-map "w" 'pdf-view-shrink)
  (evil-define-key 'normal pdf-view-mode-map "e" 'pdf-view-enlarge)
  (evil-define-key 'normal pdf-view-mode-map "Q" 'quit-window)
  (evil-define-key 'normal pdf-view-mode-map "ah" 'pdf-annot-add-highlight-markup-annotation)
  (evil-define-key 'normal pdf-view-mode-map "au" 'pdf-annot-add-underline-markup-annotation)
  (evil-define-key 'normal pdf-view-mode-map "as" 'pdf-annot-add-strikeout-markup-annotation)
  (evil-define-key 'normal pdf-view-mode-map "av" 'pdf-annot-add-squiggly-markup-annotation)
  (evil-define-key 'normal pdf-view-mode-map "am" 'pdf-annot-add-markup-annotation)
  (evil-define-key 'normal pdf-view-mode-map "4" 'pdf-view-midnight-minor-mode)
  (evil-define-key 'normal pdf-annot-list-mode-map "d" 'tablist-flag-forward)
  (evil-define-key 'normal pdf-annot-list-mode-map "u" 'tablist-unmark-forward)
  (evil-define-key 'normal pdf-annot-list-mode-map "x" 'tablist-do-flagged-delete)
  (evil-define-key 'normal pdf-annot-list-mode-map "q" 'tablist-quit)
  (evil-define-key 'normal pdf-annot-list-mode-map (kbd "RET" ) 'pdf-annot-list-display-annotation-from-id)
  (evil-define-key 'normal pdf-occur-buffer-mode-map (kbd "RET") 'pdf-occur-goto-occurrence)
  (evil-define-key 'normal pdf-outline-buffer-mode-map (kbd "RET") 'pdf-outline-display-link)
  (evil-define-key 'normal pdf-outline-buffer-mode-map "f" 'pdf-outline-follow-mode)
  (evil-define-key 'normal pdf-outline-buffer-mode-map "q" 'pdf-outline-quit)
  (evil-define-key 'normal pdf-outline-buffer-mode-map (kbd "TAB") 'pdf-outline-toggle-subtree))