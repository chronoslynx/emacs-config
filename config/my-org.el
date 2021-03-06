(defun org-pass-link-to-system (link)
  (if (string-match "^[a-zA-Z0-9]+:" link)
      (shell-command (concat "open " link))
    nil)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function to wrap blocks of text in org templates                       ;;
;; e.g. latex or src etc                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-begin-template ()
  "Make a template at point."
  (interactive)
  (if (org-at-table-p)
      (call-interactively 'org-table-rotate-recalc-marks)
    (let* ((choices '(("s" . "SRC")
                      ("e" . "EXAMPLE")
                      ("q" . "QUOTE")
                      ("v" . "VERSE")
                      ("c" . "CENTER")
                      ("l" . "LaTeX")
                      ("h" . "HTML")
                      ("a" . "ASCII")))
           (key
            (key-description
             (vector
              (read-key
               (concat (propertize "Template type: " 'face 'minibuffer-prompt)
                       (mapconcat (lambda (choice)
                                    (concat (propertize (car choice) 'face 'font-lock-type-face)
                                            ": "
                                            (cdr choice)))
                                  choices
                                  ", ")))))))
      (let ((result (assoc key choices)))
        (when result
          (let ((choice (cdr result)))
            (cond
             ((region-active-p)
              (let ((start (region-beginning))
                    (end (region-end)))
                (goto-char end)
                (insert "#+END_" choice "\n")
                (goto-char start)
                (insert "#+BEGIN_" choice "\n")))
             (t
              (insert "#+BEGIN_" choice "\n")
              (save-excursion (insert "#+END_" choice))))))))))

;;bind to key
;;(define-key org-mode-map (kbd "C-<") 'org-begin-template)

(use-package org-mode
  :commands (org-agenda org-capture org-store-link org-mode)
  :init
  (setq org-latex-pdf-process (list "latexmk -pdf -bibtex %f"))
  (setq org-directory (expand-file-name "~/Dropbox/Notes")
        ;; Local (non-synced) projects go into ~/Org
        ;; Shared orgfiles go into ~/Drobox/Notes
        org-agenda-files (list (concat org-directory "/todo.org") "~/Org")
        org-default-notes-file (concat org-directory "/todo.org")
        org-todo-keywords '((sequence "WAITING" "TODO" "|" "DONE"))
        org-agenda-include-diary t
        org-capture-templates
        '(("n" "Note" entry (file+headline (concat org-directory "/inbox.org") "Inbox")
           "* Note %<%Y-%m-%d %H:%M:%S>\n\n%?" :empty-lines 1)
          ("t" "Todo" entry (file+headline (concat org-directory "/todo.org") "Inbox")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org"))
           "* %<%H:%M>\n\n%?" :empty-lines 1))
        org-agenda-custom-commands '(
                                     ("h" "Agenda and Home-related tasks"
                                      ((agenda "")
                                       (tags-todo "home")))
                                     ("o" "Agenda and Work-related tasks"
                                      ((agenda "")
                                       (tags-todo "work")))
                                     ("w" "Waiting"
                                      ((org-agenda-sorting-strategy '(priority-down))
                                       (org-agenda-prefix-format "  Mixed: ")))
                                     )
        org-export-backends '(ascii beamer html latex md)
        org-catch-invisible-edits 'error
        org-agenda-start-on-weekday nil
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
  (add-hook 'org-open-link-functions 'org-pass-link-to-system)
  (add-hook 'org-mode-hook (lambda ()
                             (visual-line-mode)
                             (adaptive-wrap-prefix-mode)))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c s" . org-schedule)))
(use-package org-ref
  :demand
  :config
  (setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))

  ;; see org-ref for use of these variables
  (setq org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
        org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
        org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/"))
(use-package helm-bibtex
  :demand
  :config
  (setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))

  ;; see org-ref for use of these variables
  (setq org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
        org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
        org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/"))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(provide 'my-org)
