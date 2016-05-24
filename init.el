(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (smart-mode-line-respectful solarized)))
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "79a3f477ac0cb4a106f78b6109614e991564a5c2467c36e6e854d4bc1102e178" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "ca8350a6affc43fc36f84a5271e6d5278857185753cd91a899d1f88be062f77b" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(custom-theme-load-path
   (quote
    ("~/.emacs.d/el-get/smart-mode-line/themes/" "~/.emacs.d/el-get/smart-mode-line/" "~/.emacs.d/themes" "~/.emacs.d/packages/emacs-color-theme-solarized" custom-theme-directory t)))
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
;;  The following two lines thanks to https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(setq gc-cons-threshold 20000000)

(require 'cl)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
;; Packages to load
(defvar my:packages
  '(anaconda-mode
    adaptive-wrap
    avy
    ace-link
    comment-dwim-2
    company
    company-auctex
    company-irony
    company-anaconda
    dash
    deft
    edit-server
    ein
    racer
    evil
    evil-leader
    evil-matchit
    evil-nerd-commenter
    evil-org
    evil-snipe
    evil-surround
    flx
    flycheck
    flycheck-irony
    go-mode
    helm
    helm-ag
    helm-descbinds
    helm-gtags
    helm-swoop
    hydra
    key-chord
    magit
    markdown-mode
    org
    python
    rainbow-delimiters
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
    rust-mode
    sml-mode
    tramp
    web-mode
    writegood-mode
    yaml-mode
    use-package
    vagrant-tramp
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
