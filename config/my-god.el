;; Functions

(defun god-update-cursor ()
  "Update my cursor."
  (setq cursor-type
        (if god-local-mode
            'box
          'bar)))

(defun god-toggle-on-overwrite ()
  "Toggle god-mode on overwrite-mode."
  (if (bound-and-true-p overwrite-mode)
      (god-local-mode-pause)
    (god-local-mode-resume)))

;; (define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
;; (define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)

(use-package god-mode
  :commands god-mode-all god-local-mode
  :init
  (global-set-key (kbd "C-x C-1") 'delete-other-windows)
  (global-set-key (kbd "C-x C-2") 'split-window-below)
  (global-set-key (kbd "C-x C-3") 'split-window-right)
  (global-set-key (kbd "C-x C-0") 'delete-window)
  (setq god-exempt-major-modes nil)
  (setq god-exempt-predicates nil)
  :config
  ;; Keybindings
  (define-key god-local-mode-map (kbd ".") 'repeat)
  (define-key god-local-mode-map (kbd "DEL") 'delete-backward-char)
  ;; Hooks
  (add-hook 'god-mode-enabled-hook 'god-update-cursor)
  (add-hook 'god-mode-disabled-hook 'god-update-cursor)
  (add-hook 'overwrite-mode-hook 'god-toggle-on-overwrite)
  (add-to-list 'god-exempt-major-modes 'message-mode)
  (add-to-list 'god-exempt-major-modes 'magit-status-mode)
  :bind ("<escape>" . god-mode-all)
  )


(provide 'my-god)
