;; Elisp
(add-to-list 'auto-mode-alist (cons "\\.el\\'" 'emacs-lisp-mode))

;; Markdown
(defvar markdown-code-languages
    '("haskell" "lisp" "javascript" "c"))
(defun markdown-code-fence (beg end)
    "Make a code fence of the given region."
    (interactive "r")
    (save-excursion
      (goto-char beg)
      (goto-char (line-beginning-position))
      (insert "``` "
              (ido-completing-read "Language: " markdown-code-languages)
              "\n")
      (goto-char end)
      (goto-char (line-end-position))
      (newline)
      (insert "```")))
(use-package markdown-mode
  :commands (markdown-mode)
  :init
  (add-to-list 'auto-mode-alist (cons "\\.text\\'" 'markdown-mode))
    (add-to-list 'auto-mode-alist (cons "\\.txt\\'" 'markdown-mode))
  (add-to-list 'auto-mode-alist (cons "\\.md\\'" 'markdown-mode))
  (add-to-list 'auto-mode-alist (cons "\\.markdown\\'" 'markdown-mode))
  :bind-keys (:map markdown-mode-map
                   ("C-c C-f" . markdown-code-fence)
                   ("M-;" . markdown-blockquote-region))
;; C, C++
(use-package function-args
  :init (use-package cc-mode
          :config
          (defun clang-format-before-save ()
            (interactive)
            (when (or (eq major-mode 'c++-mode)
                      (eq major-mode 'c-mode))
              (clang-format-buffer)))
          (add-hook 'before-save-hook 'clang-format-before-save))
  :config
  (fa-config-default)
  (define-key c-mode-map  [(control tab)] 'moo-complete)
  (define-key c++-mode-map  [(control tab)] 'moo-complete)
  (define-key c-mode-map (kbd "M-o")  'fa-show)
  (define-key c++-mode-map (kbd "M-o")  'fa-show)
  (set-default 'semantic-case-fold t))

;; Python
(use-package 'python-mode
  :command (python-mode)
  :config
  (use-package jedi
    :config
    (use-package company-jedi
      :commands (company-jedi)
      :init
      (add-to-list 'company-backends 'company-jedi)
      (setq company-jedi-python-bin "python"))
  (use-package sphinx-doc)
  (sphinx-doc-mode t)
  (jedi:setup)
  (add-to-list 'interpreter-mode-alist '("python2" . python-mode))
  (add-to-list 'interpreter-mode-alist '("python3" . python-mode)))

;; HTML, html-templates
(use-package web-mode
  :commands (web-mode)
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.j2\\'" . web-mode)))


(use-package yaml-mode
  :commands (yaml-mode)
  :init (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))

(provide 'langs)
