(require 'deft)
(setq deft-extensions '("markdown" "md" "txt" "tex" "org"))
(setq deft-directory "~/Dropbox/Notational Velocity")
(setq deft-auto-save-interval 0.0)
(global-set-key [f8] 'deft)
(global-set-key (kbd "C-x C-g") 'deft-find-file)
(setq deft-use-filename-as-title t)

(provide 'my-deft)
