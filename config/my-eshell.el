(defun eshell-here ()
  "Opens up a new shell in the directory associated with the current buffer's file."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (name (car (last (split-string parent "/" t)))))
    (split-window-horizontally)
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(defun delete-single-window (&optional window)
  "Remove WINDOW from the display.  Default is `selected-window'.
If WINDOW is the only one in its frame, then `delete-frame' too."
  (interactive)
  (save-current-buffer
    (setq window (or window (selected-window)))
    (select-window window)
    (kill-buffer)
    (if (one-window-p t)
        (delete-frame)
      (delete-window (selected-window)))))

(defun eshell/x (&rest args)
  (delete-single-window))

(use-package em-smart)

(use-package eshell
  :init
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setenv "PAGER" "cat")
              (setenv "EDITOR" "emacsclient")))
  :config

  (setq eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-spaces-goes-to-end t)
  :bind ("C-!" . eshell-here))


(provide 'my-eshell)
