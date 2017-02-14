(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode nil)
 '(compilation-message-face (quote default))
 '(counsel-ag-base-command "/usr/local/bin/ag --nocolor --nogroup %s")
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("10e231624707d46f7b2059cc9280c332f7c7a530ebc17dba7e506df34c5332c4" "d320493111089afba1563bc3962d8ea1117dd2b3abb189aeebdc8c51b5517ddb" default)))
 '(fci-rule-color "#073642")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (cmake-mode powershell pyenv-mode-auto gruvbox-theme ggtags cargo z3-mode yaml-mode writegood-mode web-mode vagrant-tramp utop use-package tuareg sphinx-doc solarized-theme sml-mode smartparens smart-tab smart-mode-line rustfmt rainbow-delimiters racer pyenv-mode puppet-mode persp-projectile page-break-lines org-ref markdown-mode magit ivy-hydra hl-todo go-mode function-args flyspell-correct-ivy flycheck-rust flycheck-ocaml flycheck-irony flx evil-surround evil-snipe evil-org evil-nerd-commenter evil-matchit ein deft counsel-projectile counsel-osx-app counsel-dash company-irony company-auctex company-anaconda comment-dwim-2 boogie-friends better-defaults adaptive-wrap ace-link)))
 '(persp-show-modestring t)
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(safe-local-variable-values
   (quote
    ((pyenv-mode-set "apiary")
     (pyenv-mode-set "warthog"))))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#ff7f00")
     (60 . "#ffbf00")
     (80 . "#b58900")
     (100 . "#ffff00")
     (120 . "#ffff00")
     (140 . "#ffff00")
     (160 . "#ffff00")
     (180 . "#859900")
     (200 . "#aaff55")
     (220 . "#7fff7f")
     (240 . "#55ffaa")
     (260 . "#2affd4")
     (280 . "#2aa198")
     (300 . "#00ffff")
     (320 . "#00ffff")
     (340 . "#00ffff")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"])
 '(z3-solver-cmd "/usr/local/bin/z3"))
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
    boogie-friends
    comment-dwim-2
    company
    company-auctex
    company-irony
    company-anaconda
    counsel
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
    ggtags
    go-mode
    gruvbox-theme
    hl-todo
    hydra
    ivy
    ivy-hydra
    key-chord
    magit
    markdown-mode
    org
    python
    rainbow-delimiters
    smart-tab
    smartparens
    sphinx-doc
    ;; Ocaml
    utop
    tuareg
    merlin
    flycheck-ocaml
    page-break-lines
    perspective
    persp-projectile
    projectile
    puppet-mode
    pyenv-mode
    pyenv-mode-auto
    rust-mode
    sml-mode
    solarized-theme
    swiper
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
    "font"
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
