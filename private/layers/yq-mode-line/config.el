;; for fixing powerline separator issue
;; (setq-default ns-use-srgb-colorspace nil)

;; http://emacsredux.com/blog/2014/04/05/which-function-mode/
(which-function-mode)
;; when editing js file, this feature is very useful
;; (setq-default header-line-format
;;               '((which-func-mode ("" which-func-format " "))))

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" " YQ - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))

(define-fringe-bitmap 'right-curly-arrow
  [#b00000000
   #b00000000
   #b00000000
   #b00000000
   #b01110000
   #b00010000
   #b00010000
   #b00000000])

(define-fringe-bitmap 'left-curly-arrow
  [#b00000000
   #b00001000
   #b00001000
   #b00001110
   #b00000000
   #b00000000
   #b00000000
   #b00000000])
