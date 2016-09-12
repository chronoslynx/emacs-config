(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default)))
 '(safe-local-variable-values
   (quote
    ((pyenv-mode-set "apiary")
     (pyenv-mode-set "warthog")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;  The following two lines thanks to https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(setq gc-cons-threshold 20000000)

(require 'cl)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; Packages to load
(defvar my:packages
  '(anaconda-mode
    adaptive-wrap
    avy
    ace-link
    better-defaults
    comment-dwim-2
    company
    company-auctex
    company-irony
    company-anaconda
    counsel
    counsel-dash
    counsel-osx-app
    counsel-projectile
    dash
    deft
    ein
    racer
    evil
    evil-leader
    evil-matchit
    evil-nerd-commenter
    evil-org
    evil-snipe
    evil-surround
    function-args
    flx
    flycheck
    flycheck-irony
    flycheck-rust
    go-mode
    hl-todo
    hydra
    ivy
    ivy-hydra
    key-chord
    magit
    markdown-mode
    merlin
    org
    org-ref
    python
    rainbow-delimiters
    rustfmt
    smart-mode-line
    smart-tab
    smartparens
    sphinx-doc
    tuareg
    page-break-lines
    perspective
    persp-projectile
    projectile
    puppet-mode
    pyenv-mode
    rust-mode
    sml-mode
    solarized-theme
    swiper
    ;; swiper-helm
    tramp
    web-mode
    writegood-mode
    yaml-mode
    use-package
    vagrant-tramp
    yasnippet
    )
  "Packages to install")

(defvar my:configs
  '("global"
    "my-hydras"
    "my-org"
    "my-evil"
    "langs"
    "my-eshell")
  "Configuration files that follow the config/foo.el path
  format.")

;; Load packages using el-get
(defun my-packages-installed-p ()
  (loop for p in my:packages
    when (not (package-installed-p p)) do (return nil)
    finally (return t)
  )
)

(unless (my-packages-installed-p)
  (package-refresh-contents)
  (dolist (p my:packages)
    (when (not (package-installed-p p))
      (package-install p)
   )
  )
)

(eval-when-compile
  (require 'use-package))
(require 'bind-key)                ;; if you use any :bind variant

;; Load configurations
(mapc (lambda (name)
        (load (concat "~/.emacs.d/config/" name ".el")))
      my:configs)
