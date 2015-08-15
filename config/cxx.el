;; require 'function-args)
;; (fa-config-default)
;; (define-key c-mode-map  [(control tab)] 'moo-complete)
;; (define-key c++-mode-map  [(control tab)] 'moo-complete)
;; (define-key c-mode-map (kbd "M-o")  'fa-show)
;; (define-key c++-mode-map (kbd "M-o")  'fa-show)

(sp-with-modes '(c-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                            ("* ||\n[i]" "RET"))))

;; automatically format before saving in c++/c-modes
(defun clang-format-before-save ()
  (interactive)
  (when (or (eq major-mode 'c++-mode)
            (eq major-mode 'c-mode))
    (clang-format-buffer)))

(add-hook 'before-save-hook 'clang-format-before-save)
(provide 'my-cxx)
