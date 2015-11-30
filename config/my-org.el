(use-package org-mode
  :commands (org-agenda org-capture org-store-link org-mode)
  :init
  (setq org-directory (expand-file-name "~/Dropbox/Notes")
        ;; Local (non-synced) projects go into ~/Org
        ;; Shared orgfiles go into ~/Drobox/Notes
        org-agenda-files (list org-directory "~/Org")
        org-default-notes-file (concat org-directory "/inbox.org")
        org-todo-keywords '((sequence "WAITING" "TODO" "|" "DONE"))
        org-agenda-include-diary t
        org-capture-templates
        '(("n" "Note" entry (file+headline (concat org-directory "/inbox.org") "Inbox")
           "* Note %<%Y-%m-%d %H:%M:%S>\n\n%?" :empty-lines 1)
          ("t" "Todo" entry (file+headline (concat org-directory "/todo.org") "Inbox")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org"))
           "* %<%H:%M>\n\n%?" :empty-lines 1))
        org-export-backends '(ascii beamer html latex md)
        org-catch-invisible-edits 'error
        org-startup-indented t
        org-cycle-include-plain-lists 'integrate
        org-ellipsis " […]"
        org-return-follows-link t
        org-M-RET-may-split-line nil
        org-src-fontify-natively t
        org-src-preserve-indentation t
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t
        org-link-frame-setup '((file . find-file)))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c s" . org-schedule)))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(provide 'my-org)
