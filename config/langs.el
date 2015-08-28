;; Elisp
(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))

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
  :config
  (add-to-list 'auto-mode-alist '("\\.txt$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
  (bind-keys :map markdown-mode-map
             ("C-c C-f" . markdown-code-fence)
             ("M-;" . markdown-blockquote-region)))
;; C, C++
(use-package cc-mode
  :config
  (defun clang-format-before-save ()
    (interactive)
    (when (or (eq major-mode 'c++-mode)
              (eq major-mode 'c-mode))
      (clang-format-buffer)))
  (add-hook 'before-save-hook 'clang-format-before-save)
  (use-package function-args
    :load-path "packages/function-args"
    :config
    (fa-config-default)
    (define-key c-mode-map  [(control tab)] 'moo-complete)
    (define-key c++-mode-map  [(control tab)] 'moo-complete)
    (define-key c-mode-map (kbd "M-o")  'fa-show)
    (define-key c++-mode-map (kbd "M-o")  'fa-show)
    (set-default 'semantic-case-fold t)))

;; Python
(use-package sphinx-doc
  :config
  (add-to-list 'company-backends 'company-jedi))

(use-package jedi
  :demand
  :preface
  (declare-function jedi:goto-definition jedi nil)
  (declare-function jedi:related-names jedi nil)
  (declare-function jedi:show-doc jedi nil)
  :bind (("C-." . jedi:goto-definition)
	 ("C-c r" . jedi:related-names)
	 ("C-?" . jedi:show-doc)))

(use-package company-jedi
  :demand)

(use-package pyenv-mode
  :load-path "packages/pyenv-mode"
  :init (use-package pythonic
          :load-path "packages/pythonic"))

(use-package python
  :mode ("\\.py$" . python-mode)
  :interpreter ("ipython" . python-mode)
  :config
  (pyenv-mode)
  (jedi:setup)
  (sphinx-doc-mode)
  (setq python-shell-interpreter "ipython")
  (add-to-list 'company-backends 'company-jedi))

;; HTML, html-templates
(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html?" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.j2$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tmpl$" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

(provide 'langs)
