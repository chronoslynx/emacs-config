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

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global."
  (interactive "P")
  (let ((bef (downcase (or (thing-at-point 'word)
                           "")))
        aft)
    (call-interactively 'ispell-word)
    (setq aft (downcase
               (or (thing-at-point 'word) "")))
    (unless (or (string= aft bef)
                (string= aft "")
                (string= bef ""))
      (message "\"%s\" now expands to \"%s\" %sally"
               bef aft (if p "loc" "glob"))
      (define-abbrev
        (if p local-abbrev-table global-abbrev-table)
        bef aft))))

(setq save-abbrevs t)
(setq-default abbrev-mode t)

;; Global keybindings
(global-set-key (kbd "C-c C-:") 'eval-replacing-region)
(global-set-key (kbd "C-x C-k C-o") 'delete-blank-lines-in)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "<left>") 'windmove-left)
(global-set-key (kbd "<right>") 'windmove-right)
(global-set-key (kbd "<up>") 'windmove-up)
(global-set-key (kbd "<down>") 'windmove-down)
(global-set-key (kbd "C-c M-x") 'execute-extended-command)
(global-set-key (kbd "C->") 'end-of-buffer)
(global-set-key (kbd "C-<") 'beginning-of-buffer)
(global-set-key (kbd "C-!") 'eval-defun)
(define-key ctl-x-map "\C-i" 'endless/ispell-word-then-abbrev)

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
(use-package remember
  :commands (remember)
  :config (define-key remember-notes-mode-map (kbd "C-c C-c") nil)
  :bind ("C-c r" . remember))

(use-package projectile
  :config
  (projectile-global-mode))

(use-package flycheck
  :commands (global-flycheck-mode)
  :init (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package yasnippet
  :config (yas-global-mode 1))

(use-package smartparens
  :config
  (use-package smartparens-config)
  (smartparens-global-mode t)
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'minibuffer-inactive-mode "`" nil :actions nil)
  ;; Lisps
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "`" nil :actions nil)
  (sp-local-pair 'scheme-mode "'" nil :actions nil)
  (sp-local-pair 'scheme-mode "`" nil :actions nil)
  (sp-local-pair 'inferior-scheme-mode "'" nil :actions nil)
  (sp-local-pair 'inferior-scheme-mode "`" nil :actions nil)
  ;; *TeX
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
  (sp-local-pair 'tex-mode "`" nil :actions nil)
  (sp-with-modes '(c-mode c++-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                              ("* ||\n[i]" "RET")))))
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

(use-package
  helm
  :config
  (use-package helm-config)
  (use-package helm-gtags
    :commands (helm-gtags-mode)
    :init
    (add-hook 'dired-mode-hook 'helm-gtags-mode)
    (add-hook 'eshell-mode-hook 'helm-gtags-mode)
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'rust-mode-hook 'helm-gtags-mode)
    (add-hook 'asm-mode-hook 'helm-gtags-mode)
    (setq
     helm-gtags-ignore-case t
     helm-gtags-auto-update t
     helm-gtags-use-input-at-cursor t
     helm-gtags-pulse-at-cursor t
     helm-gtags-prefix-key "\C-cg"
     helm-gtags-suggested-key-mapping t)
    :bind-keys (:map helm-gtags-mode-map
                     ("C-c g a" . helm-gtags-tags-in-this-function)
                     ("C-j" . helm-gtags-select)
                     ("M-." . helm-gtags-dwim)
                     ("M-," . helm-gtags-pop-stack)
                     ("C-c <" . helm-gtags-previous-history)
                     ("C-c >" . helm-gtags-next-history))

  (use-package helm-projectile
    :commands (helm-projectile-on)
    :config (setq projectile-completion-system 'helm))
  (use-package swiper-helm
    :bind (("\C-s" . swiper-helm)
           ("\C-s" . swiper-helm)
           ("C-c C-r" . helm-resume)))
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
  :bind (("M-x" . helm-M-x)
         ("C-x C-m" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-h f" . helm-apropos)
         ("C-h r" . helm-info-emacs)
         ("C-h C-l" . helm-locate-library)
         ("C-c h" . helm-command-prefix)))
(global-unset-key (kbd "C-x c"))

;; Company
(use-package company
  :commands (global-company-mode)
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (use-package helm-company
    :commands (helm-company)
    :init
    (define-key company-mode-map (kbd "C-:") 'helm-company)
    (define-key company-active-map (kbd "C-:") 'helm-company))
  :config
  (setq company-backends (delete 'company-capf company-backends))
  (setq company-tooltip-flip-when-above t
        company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-numbers t
        company-require-match 'never
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t))

(use-package comment-dwim-2
  :bind ("C-;" . comment-dwim-2))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode)
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package magit
  :commands (magit-status)
  :init
  (setq magit-revert-buffers 'silent)
  (global-set-key [f10] 'magit-status)
  :config
  (use-package info
    :config
    (info-initialize)
    (add-to-list 'Info-directory-list
                 "~/.emacs.d/packages/magit/Documentation/"))))

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
  (add-to-list 'aggressive-indent-excluded-modes 'python-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

;; Deft
(use-package deft
  :commands (deft deft-find-file)
  :init
  (setq deft-extensions '("markdown" "md" "txt" "tex" "org"))
  (setq deft-directory "~/Dropbox/Notational Velocity/")
  (setq deft-auto-save-interval 0.0)
  (setq deft-use-filename-as-title t)
  (global-set-key [f8] 'deft)
  :config
  :bind ("C-c C-d" . deft-find-file))

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
