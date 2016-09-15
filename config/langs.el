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
      '(add-to-list 'company-backends 'company-irony))))

;; Python
(use-package sphinx-doc
  :demand
  :commands sphinx-doc-mode)

;; (use-package jedi
;;   :demand
;;   :preface
;;   (declare-function jedi:goto-definition jedi nil)
;;   (declare-function jedi:related-names jedi nil)
;;   (declare-function jedi:show-doc jedi nil)
;;   :bind (("C-." . jedi:goto-definition)
;; 	 ("C-c r" . jedi:related-names)
;; 	 ("C-?" . jedi:show-doc)))

;; (use-package company-jedi
;;   :demand)
(use-package company-anaconda
  :demand
  :config
  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-anaconda)))

(use-package pyenv-mode
  :init (use-package pythonic)
  :config
  (defun projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((project (projectile-project-name)))
      (if (member project (pyenv-mode-versions))
          (pyenv-mode-set project)
        (pyenv-mode-unset))))

  (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set))

(use-package anaconda-mode
  :demand)


(use-package python
  :mode ("\\.py$" . python-mode)
  :config
  (pyenv-mode)
  (add-hook 'python-mode-hook (lambda ()
                                (require 'sphinx-doc)
                                (anaconda-mode)
                                (sphinx-doc-mode t))))


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
  (setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
   ;; Setup environment variables using opam
  (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var)))
  (use-package utop
    :ensure)
  (autoload 'utop "utop" "Toplevel for OCaml" t)
  (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
  (add-hook 'tuareg-mode-hook 'utop-minor-mode)
  )

(provide 'langs)
