;; el-get setup
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(setq el-get-user-package-directory "~/.emacs.d/init-files")
(require 'el-get nil 'noerror)
(el-get-bundle dash)
(require 'dash)

;; Packages to load
(defvar my:elpackages
  '(company-mode
    company-auctex
    company-irony
    company-jedi
    flx
    flycheck
    flycheck-puppet
    go-mode
    god-mode
    helm
    helm-swoop
    magit
    markdown-mode
    rainbow-delimiters
    projectile
    puppet-mode
    rust-mode
    sml-mode
    )
  "Packages to install via el-get")

(defvar packages
  '(better-defaults
    flycheck-pylama)
  "Packages to install locally from packages/name/name.el")

(defvar configs
  '("global"
    "god"
    "golang"
    "haskell"
    "my-helm"
    "markdown"
    "my-magit"
    "rust"
    "python")
  "Configuration files that follow the config/foo.el path
  format.")

;; Load packages using el-get
(el-get 'sync my:elpackages)

(let (s)
  (-each packages
    (lambda (name)
       (progn (unless (fboundp name)
		(add-to-list 'load-path
			     (concat "~/.emacs.d/packages/"
				     (symbol-name name)))
		(require name))))))

;; Load configurations
(let (s)
  (-each configs
    (lambda (name)
      (load (concat "~/.emacs.d/config/"
                    name ".el")))))

;; Mode initializations
(require 'helm-config)
(projectile-global-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)
(god-mode)
