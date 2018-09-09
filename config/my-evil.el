(use-package evil-leader
  :commands global-evil-leader-mode
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "b" 'switch-to-buffer
    "c" 'compile
    "e" 'counsel-find-file
    "d" 'kill-buffer
    "f" 'counsel-ag
    "g" 'hydra-gtags/body
    "h" 'hydra-describe/body
    "l" 'counsel-locate
    "m" 'counsel-imenu
    "p" 'hydra-projectile/body
    "q" 'hydra-flycheck/body
    "r" 'ivy-resume
    "s" 'swiper
    "w" 'save-buffer
    "x" 'hydra-windows/body
    "y" 'counsel-yank-pop
    "<SPC>" 'counsel-M-x
    ";" 'avy-goto-word-1
    )
  )

(use-package key-chord
  :commands key-chord-mode)
(use-package evil-surround
  :commands global-evil-surround-mode)
(use-package evil-matchit
  :commands global-evil-matchit-mode)
(use-package evil-snipe
  :commands evil-snipe-override-mode
  :config
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))

(defun my-save-if-bufferfilename ()
  (if (buffer-file-name) (progn (save-buffer))
    (message "no file is associated to this buffer: do nothing")))

(use-package evil-org)
(use-package evil
  :commands evil-mode
  :config
  (add-hook 'evil-insert-state-exit-hook 'my-save-if-bufferfilename)
  (setq evil-insert-state-modes (append evil-motion-state-modes '(deft-mode)))
   ;;; esc quits
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  ;; Emacs movement keys in insert mode
  (define-key evil-insert-state-map "\C-a" 'evil-beginning-of-line)
  (define-key evil-insert-state-map "\C-e" 'end-of-line)
  (define-key evil-insert-state-map "\C-n" 'evil-next-line)
  (define-key evil-insert-state-map "\C-p" 'evil-previous-line)
  (define-key evil-insert-state-map "\C-d" 'evil-delete)
  ;; Exit insert mode on jk
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (evil-snipe-override-mode 1)
  (global-evil-surround-mode 1)
  (global-evil-matchit-mode 1))
(global-evil-leader-mode)
(evil-mode 1)
(provide 'my-evil)
