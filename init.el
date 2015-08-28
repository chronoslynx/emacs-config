(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (smart-mode-line-dark gruvbox)))
 '(custom-safe-themes
   (quote
    ("79a3f477ac0cb4a106f78b6109614e991564a5c2467c36e6e854d4bc1102e178" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "ca8350a6affc43fc36f84a5271e6d5278857185753cd91a899d1f88be062f77b" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(custom-theme-load-path
   (quote
    ("~/.emacs.d/el-get/smart-mode-line/themes/" "~/.emacs.d/el-get/smart-mode-line/" "~/.emacs.d/themes" custom-theme-directory t)))
 '(helm-ag-base-command "pt -e --nocolor --nogroup")
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

;; el-get setup
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get nil 'noerror)

;; Packages to load
(defvar my:elpackages
  '(aggressive-indent-mode
    comment-dwim-2
    company-mode
    company-auctex
    company-irony
    company-jedi
    clang-format
    dash
    deft
    ein
    flx
    flycheck
    go-mode
    helm
    helm-ag
    helm-descbinds
    helm-gtags
    helm-swoop
    hydra
    jedi
    magit
    markdown-mode
    org-mode
    python
    rainbow-delimiters
    smart-mode-line
    smartparens
    sphinx-doc
    perspective
    projectile
    puppet-mode
    rust-mode
    sml-mode
    tramp
    web-mode
    yaml-mode
    use-package
    vagrant-tramp
    )
  "Packages to install via el-get")

(defvar my:configs
  '("global"
    "my-hydras"
    "langs"
    "my-eshell")
  "Configuration files that follow the config/foo.el path
  format.")

;; Load packages using el-get
(el-get 'sync my:elpackages)
(eval-when-compile
  (require 'use-package))
(require 'bind-key)                ;; if you use any :bind variant

;; Load configurations
(mapc (lambda (name)
        (load (concat "~/.emacs.d/config/" name ".el")))
      my:configs)
