;; Requirements
(require 'uniquify)


(require 'yaml-mode)

;; Fundamental functions

(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))

(defun delete-blank-lines-in (start end)
  "Delete blank lines at point or in the region."
  (interactive "r")
  (replace-regexp "[\n]+" "\n" nil start end))

(defun eval-replacing-region (read)
  "Eval an expression on the region and replace the region with the
  result."
  (interactive "P")
  (unless (region-active-p)
    (error "Region is not active!"))
  (let* ((string
          (buffer-substring-no-properties
           (region-beginning)
           (region-end)))
         (function
          (eval
           `(lambda (x)
              ,(read-from-minibuffer "Expression on x: " "" nil t))))
         (result (funcall function (if read (read string) string)))
         (start (point)))
    (delete-region (region-beginning)
                   (region-end))
    (insert (case (type-of result)
              (string (format "%s" result))
              (t (format "%S" result))))
    (set-mark (point))
    (goto-char start)))

(defun auto-chmod ()
  "If we're in a script buffer, then chmod +x that script."
  (and (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (save-match-data
             (looking-at "^#!"))))
       (shell-command (concat "chmod u+x " buffer-file-name))
       (message (concat "Saved as script: " buffer-file-name))))

(defun insert-date ()
  (interactive)
  (insert (shell-command-to-string "date +'%Y-%m-%d'")))

;; Global keybindings
(global-set-key (kbd "C-c C-:") 'eval-replacing-region)
(global-set-key (kbd "C-x C-k C-o") 'delete-blank-lines-in)
(global-set-key (kbd "M-g") 'goto-line)

(global-set-key (kbd "<left>") 'windmove-left)
(global-set-key (kbd "<right>") 'windmove-right)
(global-set-key (kbd "<up>") 'windmove-up)
(global-set-key (kbd "<down>") 'windmove-down)

(global-set-key (kbd "C-c M-x") 'execute-extended-command)
(global-set-key (kbd "C-c r") 'remember)
(with-eval-after-load 'remember
  (define-key remember-notes-mode-map (kbd "C-c C-c") nil))

(global-set-key (kbd "C->") 'end-of-buffer)
(global-set-key (kbd "C-<") 'beginning-of-buffer)
(global-set-key (kbd "C-!") 'eval-defun)


;; Autoloads
(add-to-list 'auto-mode-alist (cons "\\.el\\'" 'emacs-lisp-mode))


(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; Environment settings

(set-language-environment "UTF-8")

(fset 'yes-or-no-p 'y-or-n-p)
;; GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t
 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;; font and color
(defun fontify-frame (frame)
  (set-frame-parameter frame 'font "Input-14"))
(fontify-frame nil)
(push 'fontify-frame after-make-frame-functions)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'gruvbox t)

;; Other Global Setup
(use-package projectile
  :config
  (projectile-global-mode))

(use-package flycheck
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package yasnippet
  :config (yas-global-mode 1))

(use-package smartparens
  :config
  (use-package smartparens-config)
  (smartparens-global-mode t)
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'minibuffer-inactive-mode "`" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "`" nil :actions nil)
  (sp-local-pair 'scheme-mode "'" nil :actions nil)
  (sp-local-pair 'scheme-mode "`" nil :actions nil)
  (sp-local-pair 'inferior-scheme-mode "'" nil :actions nil)
  (sp-local-pair 'inferior-scheme-mode "`" nil :actions nil)

  (sp-local-pair 'LaTeX-mode "\"" nil :actions nil)
  (sp-local-pair 'LaTeX-mode "'" nil :actions nil)
  (sp-local-pair 'LaTeX-mode "`" nil :actions nil)
  (sp-local-pair 'latex-mode "\"" nil :actions nil)
  (sp-local-pair 'latex-mode "'" nil :actions nil)
  (sp-local-pair 'latex-mode "`" nil :actions nil)
  (sp-local-pair 'TeX-mode "\"" nil :actions nil)
  (sp-local-pair 'TeX-mode "'" nil :actions nil)
  (sp-local-pair 'TeX-mode "`" nil :actions nil)
  (sp-local-pair 'tex-mode "\"" nil :actions nil)
  (sp-local-pair 'tex-mode "'" nil :actions nil)
  (sp-local-pair 'tex-mode "`" nil :actions nil))

;; Mode line
(use-package smart-mode-line
  :config
  (sml/setup)
  (setq rm-blacklist ".*")
  (add-to-list 'sml/replacer-regexp-list '("^~/Projects/\\(\\w+\\)/"
                                           (lambda(s) (concat ":" (upcase (match-string 1 s)) ":"))
                                           ) t)
  (add-to-list 'sml/replacer-regexp-list '("^~/Development/" ":DEV:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/" ":DB:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/Class/" ":CLS:") t))

;; Company
(with-eval-after-load 'company
  (add-hook 'after-init-hook 'global-company-mode)
  (define-key company-active-map (kbd "C-:") 'helm-company)
  (setq company-backends (delete 'company-capf company-backends))
  (setq company-tooltip-flip-when-above t
        company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-numbers t
        company-require-match 'never
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t
        company-jedi-python-bin "python"))

(use-package comment-dwim-2
  :bind ("C-;" . comment-dwim-2))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package hl-todo
  :config
  (setq hl-todo-keyword-faces '(("TODO" . hl-todo)
                                ("NOTE" . hl-todo)
                                ("FIXME" . hl-todo)
                                ("KLUDGE" . hl-todo)))
  (hl-todo-set-regexp)
  (global-hl-todo-mode))

(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)

  (defvar my:unaggressive-modes
    '("python"
      "html"
      )
    "Modes in which to not aggressively indent")

  (mapc
   (lambda (mode)
     (add-to-list 'aggressive-indent-excluded-modes
                  (intern (concat mode "-mode"))))
   my:unaggressive-modes))


;; Decrease keystroke echo timeout
(setq echo-keystrokes 0.5)
(setq line-number-display-limit-width 10000)

;; Backups
(setq vc-make-backup-files t)
(setq version-control t ;; Use version numbers for backups.
      kept-new-versions 10 ;; Number of newest versions to keep.
      kept-old-versions 0 ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t) ;; Copy all files, don't rename them.
;; Default and per-save backups go here:
(setq backup-directory-alist '(("" . "~/.emacs.d/backup/per-save")))
(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))
(add-hook 'before-save-hook 'force-backup-of-buffer)



(provide 'global)
