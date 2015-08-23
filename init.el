(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "ca8350a6affc43fc36f84a5271e6d5278857185753cd91a899d1f88be062f77b" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(helm-ag-base-command "pt -e --nocolor --nogroup"))
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
    flx
    flycheck
    go-mode
    helm
    helm-ag
    helm-gtags
    helm-swoop
    hydra
    jedi
    magit
    markdown-mode
    org-mode
    rainbow-delimiters
    smart-mode-line
    smartparens
    sphinx-doc
    projectile
    puppet-mode
    rust-mode
    sml-mode
    web-mode
    yaml-mode
    use-package
    )
  "Packages to install via el-get")

(defvar my:packages
  '(better-defaults
    flycheck-pylama
    function-args
    hl-todo
    swiper
    swiper-helm
    yasnippet
    )
  "Packages to install locally from packages/name/name.el")

(defvar my:configs
  '("global"
    "my-hydras"
    "langs"
    "my-org")
  "Configuration files that follow the config/foo.el path
  format.")

;; Load packages using el-get
(el-get 'sync my:elpackages)
(eval-when-compile
  (require 'use-package))
(require 'bind-key)                ;; if you use any :bind variant
(mapc (lambda (name)
        (progn (unless (fboundp name)
                 (add-to-list 'load-path
                              (concat "~/.emacs.d/packages/"
                                      (symbol-name name)))
                 (require name))))
      my:packages)

;; Load configurations
(mapc (lambda (name) (load (concat "~/.emacs.d/config/" name ".el"))) my:configs)
