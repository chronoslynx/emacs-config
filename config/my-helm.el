(require 'helm-config)
(require 'swiper-helm)
(helm-mode 1)
(helm-adaptive-mode 1)
(helm-autoresize-mode 1)
(helm-push-mark-mode 1)

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h f") 'helm-apropos)
(global-set-key (kbd "C-h r") 'helm-info-emacs)
(global-set-key (kbd "C-h C-l") 'helm-locate-library)

(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
(substitute-key-definition 'find-tag 'helm-etags-select global-map)
(setq projectile-completion-system 'helm)

(global-set-key "\C-s" 'swiper-helm)
(global-set-key "\C-r" 'swiper-helm)
(global-set-key (kbd "C-c C-r") 'helm-resume)

(helm-projectile-on)
(provide 'my-helm)
