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
    smartparens
    sphinx-doc
    projectile
    puppet-mode
    rust-mode
    sml-mode
    yaml-mode
    )
  "Packages to install via el-get")

(defvar my:packages
  '(better-defaults
    flycheck-pylama
    function-args
    swiper
    swiper-helm
    yasnippet
    )
  "Packages to install locally from packages/name/name.el")

(defvar my:configs
  '("global"
    "cxx"
    "my-aggressive-indent"
    "my-deft"
    "my-eshell"
    "my-ggtags"
    "golang"
    "haskell"
    "my-helm"
    "my-hydras"
    "markdown"
    "my-magit"
    "my-org"
    "rust"
    "python")
  "Configuration files that follow the config/foo.el path
  format.")

;; Load packages using el-get
(el-get 'sync my:elpackages)

(mapc (lambda (name)
        (progn (unless (fboundp name)
                 (add-to-list 'load-path
                              (concat "~/.emacs.d/packages/"
                                      (symbol-name name)))
                 (require name))))
      my:packages)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("ca8350a6affc43fc36f84a5271e6d5278857185753cd91a899d1f88be062f77b" default)))
 '(helm-ag-base-command "pt -e --nocolor --nogroup"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Load configurations
(mapc (lambda (name) (load (concat "~/.emacs.d/config/" name ".el"))) my:configs)
