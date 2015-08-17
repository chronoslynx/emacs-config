(setq org-directory (expand-file-name "~/Dropbox/org")
      org-agenda-files (list org-directory)
      org-default-notes-file (concat org-directory "/inbox.org")
      org-capture-templates
      '(("n" "Note" entry (file+headline (concat org-directory "/inbox.org") "Inbox")
         "* TODO %<%Y-%m-%d %H:%M:%S>\n\n%?" :empty-lines 1)
        ("p" "PW" entry (file+headline (concat org-directory "/pw.org") "PW")
         "* TODO %<%Y-%m-%d %H:%M:%S>\n\n%?" :empty-lines 1)
        ("w" "Work" entry (file+datetree (concat org-directory "/work.org"))
         "* %<%H:%M>\n\n%?" :empty-lines 1)
        ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org"))
         "* %<%H:%M>\n\n%?" :empty-lines 1)))
(setq org-export-backends '(ascii beamer html latex md))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

(setq org-catch-invisible-edits 'error
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
(provide 'my-org)
