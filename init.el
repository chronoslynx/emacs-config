;; (Shamelessly based on chrisdone's emacs config)
;; Standard libraries needed
(add-to-list 'load-path "~/.emacs.d/packages/dash")
(require 'dash)

;; Packages to laod
(defvar packages
  '(auto-complete
    better-defaults
    company
    company-jedi
    flycheck
    flycheck-pylama
    markdown-mode
    projectile
    rainbow-delimeters
    ;rust-mode
    sml-mode
    )
  "Packages whose location follows the
  packages/package-name/package-name.el format.")

(defvar configs
  '("global"
    "god"
    "golang"
    "haskell"
    "prelude-helm-everywhere"
    "markdown"
    "my-magit"
    "rust"
    "python")
  "Configuration files that follow the config/foo.el path
  format.")

(defvar custom-load-paths
  '("company-mode"
    "ctable"
    "emacs-async"
    "emacs-deferred"
    "epc"
    "flx"
;    "jedi"
    "god-mode"
    "helm"
    "helm-projectile"
    "magit/lisp"))

;; Add custom load paths
(let (s)
  (-each custom-load-paths
    (lambda (location)
        (add-to-list 'load-path
	       (concat (file-name-directory (or load-file-name
						(buffer-file-name)))
		       "packages/"
		       location)))))
;; Load packages
(let (s)
  (-each packages
    (lambda (name)
           (progn (unless (fboundp name)
                    (add-to-list 'load-path
                                 (concat (file-name-directory (or load-file-name
                                                                  (buffer-file-name)))
                                         "packages/"
                                         (symbol-name name)))
                    (require name))))))

;; Load configurations
(let (s)
  (-each configs
    (lambda (name)
      (load (concat (file-name-directory load-file-name)
                    "config/"
                    name ".el")))))

;; Mode initializations
(require 'helm-config)
(projectile-global-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)
(god-mode)
