(require 'helm)

(use-package
 helm
 :config
 (use-package helm-config)
 (use-package helm-gtags
              :config
              (setq
               helm-gtags-ignore-case t
               helm-gtags-auto-update t
               helm-gtags-use-input-at-cursor t
               helm-gtags-pulse-at-cursor t
               helm-gtags-prefix-key "\C-cg"
               helm-gtags-suggested-key-mapping t)
              (add-hook 'dired-mode-hook 'helm-gtags-mode)
              (add-hook 'eshell-mode-hook 'helm-gtags-mode)
              (add-hook 'c-mode-hook 'helm-gtags-mode)
              (add-hook 'c++-mode-hook 'helm-gtags-mode)
              (add-hook 'asm-mode-hook 'helm-gtags-mode)
              (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
              (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
              (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
              (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
              (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
              (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))
 (use-package helm-projectile
              :config
              (setq projectile-completion-system 'helm))
 (use-package swiper-helm
              :config
              (global-set-key "\C-s" 'swiper-helm)
              (global-set-key "\C-r" 'swiper-helm)
              (global-set-key (kbd "C-c C-r") 'helm-resume))
 (helm-mode 1)
 (helm-adaptive-mode 1)
 (helm-autoresize-mode 1)
 (helm-push-mark-mode 1)
 (helm-projectile-on)
 (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
       helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
       helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
       helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
       helm-ff-file-name-history-use-recentf t)

 (setq helm-split-window-in-side-p           t
       helm-buffers-fuzzy-matching           t
       helm-move-to-line-cycle-in-source     t
       helm-ff-search-library-in-sexp        t
       helm-ff-file-name-history-use-recentf t)
 (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
 (substitute-key-definition 'find-tag 'helm-etags-select global-map)
 :bind
 ("M-x" . helm-M-x)
 ("C-x C-m" . helm-M-x)
 ("M-y" . helm-show-kill-ring)
 ("C-x b" . helm-mini)
 ("C-x C-b" . helm-buffers-list)
 ("C-x C-f" . helm-find-files)
 ("C-h f" . helm-apropos)
 ("C-h r" . helm-info-emacs)
 ("C-h C-l" . helm-locate-library)
 ("C-c h" . helm-command-prefix))

(global-unset-key (kbd "C-x c"))

(provide 'my-helm)
