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
(use-package ggtags
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                (require 'semantic)
                (global-semanticdb-minor-mode 1)
                (global-semantic-idle-scheduler-mode 1)
                (semantic-mode 1)
                (ggtags-mode 1)))))

(use-package irony
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (use-package flycheck-irony
    :config
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))
  :config
  (use-package company-irony
    :config
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-semantic))
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-irony))))

;; Python
(use-package python
  :mode ("\\.py$" . python-mode)
  :init
  (use-package sphinx-doc
    :commands sphinx-doc-mode)
  (use-package anaconda-mode)
  :config
  (add-hook 'python-mode-hook (lambda ()
                                (require 'sphinx-doc)
                                (anaconda-mode)
                                (sphinx-doc-mode t)))
  )


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

;; Golang
(use-package go-mode
  :mode ("\\go$" . go-mode)
  :config (add-hook 'before-save-hook 'gofmt-before-save)
  :bind (("C-." . godef-jump)))

(use-package rust-mode
  :config
  :mode ("\\rs$" . rust-mode))

(use-package boogie-friends
  :mode ("\\z3$" . z3-smt2-mode))

(use-package merlin
  :ensure
  :config
  (add-hook 'merlin-mode-hook 'company-mode)
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'merlin-company-backend))
  (add-hook 'tuareg-mode-hook 'merlin-mode))

(use-package tuareg
  :init
  :config
  (require 'merlin)
   ;; Setup environment variables using opam
  ;; (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
  ;;   (setenv (car var) (cadr var)))
  (use-package utop
    :ensure)
  (autoload 'utop "utop" "Toplevel for OCaml" t)
  (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
  (add-hook 'tuareg-mode-hook 'utop-minor-mode)
  :mode ("\\.ml\\w?" . tuareg-mode)
  )

(provide 'langs)
