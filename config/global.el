;; Fundamental functions
(add-hook 'after-init-hook (lambda () (load-theme 'solarized-dark)))
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

(defun dcaps-to-scaps ()

  "Convert word in DOuble CApitals to Single Capitals."
  (interactive)
  (and (= ?w (char-syntax (char-before)))
       (save-excursion
         (and (if (called-interactively-p)
                  (skip-syntax-backward "w")
                (= -3 (skip-syntax-backward "w")))
              (let (case-fold-search)
                (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
              (capitalize-word 1)))))

(define-minor-mode dubcaps-mode
  "Toggle `dubcaps-mode'.  Converts words in DOuble CApitals to
Single Capitals as you type."
  :init-value nil
  :lighter (" DC")
  (if dubcaps-mode
      (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'dcaps-to-scaps 'local)))

(defun my-split-window-horizontally ()
  "Split window with another buffer."
  (interactive)
  (select-window (split-window-horizontally))
  (switch-to-buffer (other-buffer)))


;; Global keybindings
(define-key ctl-x-map "\C-i" 'endless/ispell-word-then-abbrev)
(global-set-key (kbd "C-x 3") 'my-split-window-horizontally)

;; Ensure that page nav doesn't leave the cursor at the bottom
(advice-add #'backward-page :after #'recenter)
(advice-add #'forward-page  :after #'recenter)
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

;; font
(defun fontify-frame (frame)
  (set-frame-parameter frame 'font "Fira Code Retina-14"));;"Fira Code-14"))
(fontify-frame nil)
(push 'fontify-frame after-make-frame-functions)

;; Hooks
(add-hook 'text-mode-hook #'dubcaps-mode)

;; Other Global Setup
(use-package better-defaults)

(use-package remember
  :config (define-key remember-notes-mode-map (kbd "C-c C-c") nil)
  :bind ("C-c r" . remember))

(use-package perspective
  :config
  (persp-mode)
  (require 'persp-projectile)
  (define-key projectile-mode-map (kbd "s-s") 'projectile-persp-switch-project))

(use-package projectile
  :config
  (projectile-global-mode))

(use-package flycheck
  :init (add-hook 'after-init-hook 'global-flycheck-mode)
  :config
  (use-package flycheck-pylama
    :load-path "packages/flycheck-pylama"))

(use-package yasnippet
  :config
  (yas-global-mode 1))

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
  ;;  Ocaml
  (sp-local-pair 'tuareg-mode "'" nil :actions nil)
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
  ;; Rust
  (sp-local-pair 'rust-mode "<" ">")
  (sp-local-pair 'rust-mode "|" "|")
  (sp-with-modes '(c-mode c++-mode rust-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                              ("* ||\n[i]" "RET")))))
;; Mode line
(use-package smart-mode-line
  :config
  (setq sml/theme 'respectful)
  (sml/setup)
  ;; (setq rm-blacklist ".*")
  (add-to-list 'sml/replacer-regexp-list '("^~/Projects/\\(\\w+\\)/"
                                           (lambda(s) (concat ":" (upcase (match-string 1 s)) ":"))
                                           ) t)
  (add-to-list 'sml/replacer-regexp-list '("^~/Development/" ":DEV:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/" ":DB:") t)
  (add-to-list 'sml/replacer-regexp-list '("^~/Dropbox/Class/" ":CLS:") t))

;; Helm the Mighty
(use-package helm
  :demand
  :config
  (use-package helm-config)
  (helm-mode 1)
  (helm-adaptive-mode 1)
  (helm-autoresize-mode 1)
  (helm-push-mark-mode 1)
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
  (use-package helm-gtags
    :commands (helm-gtags-mode)
    :init
    (add-hook 'dired-mode-hook 'helm-gtags-mode)
    (add-hook 'eshell-mode-hook 'helm-gtags-mode)
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'rust-mode-hook 'helm-gtags-mode)
    (add-hook 'asm-mode-hook 'helm-gtags-mode)
    :config
    (setq
     helm-gtags-ignore-case t
     helm-gtags-auto-update t
     helm-gtags-use-input-at-cursor t
     helm-gtags-pulse-at-cursor t
     helm-gtags-prefix-key "\C-c g"
     helm-gtags-suggested-key-mapping t)
    (bind-keys :map helm-gtags-mode-map
               ("C-c g a" . helm-gtags-tags-in-this-function)
               ("C-j" . helm-gtags-select)
               ("M-." . helm-gtags-dwim)
               ("M-," . helm-gtags-pop-stack)
               ("C-c <" . helm-gtags-previous-history)
               ("C-c >" . helm-gtags-next-history)))
  (use-package helm-projectile
    :config
    (helm-projectile-on)
    (setq projectile-completion-system 'helm))

  (add-hook 'eshell-mode-hook
            '(lambda ()
               (define-key eshell-mode-map (kbd "TAB") 'helm-esh-pcomplete)
               (define-key eshell-mode-map (kbd "C-c C-l") 'helm-eshell-history)))
  :bind (("M-x" . helm-M-x)
         ("C-x C-m" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-h a" . helm-apropos)
         ("C-h i" . helm-info-emacs)
         ("C-h C-l" . helm-locate-library)
         ("C-c h" . helm-command-prefix)))

(use-package helm-pages)

(use-package swiper-helm
  :init (use-package swiper)
  :bind (("\C-s" . swiper-helm)
         ("\C-s" . swiper-helm)
         ("C-c C-r" . helm-resume)))
(use-package helm-descbinds
  :bind (("C-h b" . helm-descbinds)))

(global-unset-key (kbd "C-x c"))

;; Company
(use-package company
  ;; :init (global-auto-complete-mode 0)
  :config
  (setq company-tooltip-flip-when-above t
        company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-numbers t
        company-require-match 'never
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t)
  (global-company-mode))

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
  :config;; make magit work
  (dolist (map (list
                ;; Mode maps
                magit-blame-mode-map
                magit-cherry-mode-map
                magit-diff-mode-map
                magit-log-mode-map
                magit-log-select-mode-map
                magit-mode-map
                ;; No evil keys for the popup.
                ;; magit-popup-help-mode-map
                ;; magit-popup-mode-map
                ;; magit-popup-sequence-mode-map
                magit-process-mode-map
                magit-reflog-mode-map
                magit-refs-mode-map
                magit-revision-mode-map
                magit-stash-mode-map
                magit-stashes-mode-map
                magit-status-mode-map
                ;; Section submaps
                magit-branch-section-map
                magit-commit-section-map
                magit-file-section-map
                magit-hunk-section-map
                magit-module-commit-section-map
                magit-remote-section-map
                magit-staged-section-map
                magit-stash-section-map
                magit-stashes-section-map
                magit-tag-section-map
                magit-unpulled-section-map
                magit-unpushed-section-map
                magit-unstaged-section-map
                magit-untracked-section-map))
    ;; Move current bindings for movement keys to their upper-case counterparts.
    (dolist (key (list "k" "j" "h" "l"))
      (let ((binding (lookup-key map key)))
        (when binding
          (define-key map (upcase key) binding) (define-key map key nil))))
    (evil-add-hjkl-bindings map 'emacs
      (kbd "v") 'evil-visual-char
      (kbd "V") 'evil-visual-line
      (kbd "C-v") 'evil-visual-block
      (kbd "C-w") 'evil-window-map))
  (dolist (mode (list 'magit-blame-mode
                      'magit-cherry-mode
                      'magit-diff-mode
                      'magit-log-mode
                      'magit-log-select-mode
                      'magit-mode
                      'magit-popup-help-mode
                      'magit-popup-mode
                      'magit-popup-sequence-mode
                      'magit-process-mode
                      'magit-reflog-mode
                      'magit-refs-mode
                      'magit-revision-mode
                      'magit-stash-mode
                      'magit-stashes-mode
                      'magit-status-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  (use-package info
    :config
    (info-initialize)
    (add-to-list 'Info-directory-list
                 "~/.emacs.d/packages/magit/Documentation/")))

(use-package hl-todo
  :init (setq hl-todo-activate-in-modes '(prog-mode))
  :config
  (setq hl-todo-keyword-faces '(("TODO" . hl-todo)
                                ("NOTE" . hl-todo)
                                ("FIXME" . hl-todo)
                                ("KLUDGE" . hl-todo)))
  (hl-todo-set-regexp))
(global-hl-todo-mode)

(use-package page-break-lines)
(global-page-break-lines-mode)

(use-package ace-link
  :config (ace-link-setup-default))

;; Deft
(use-package deft
  :commands (deft deft-find-file)
  :init
  (setq deft-extensions '("org"))
  (setq deft-directory "~/Dropbox/Notes/")
  (setq deft-text-mode 'org-mode)
  (setq deft-auto-save-interval 0.0)
  (setq deft-use-filename-as-title t)
  (global-set-key [f8] 'deft)
  :bind ("C-c C-d" . deft-find-file))

;; Tramp
(use-package vagrant-tramp
  :commands vagrant-tramp-enable)
(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (vagrant-tramp-enable))

;; Shut projectile up when using tramp
(add-hook 'find-file-hook
          (lambda ()
            (when (file-remote-p default-directory)
              (setq-local projectile-mode-line "Projectile"))))

(use-package server)

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

(use-package writegood-mode
  :demand)
(global-set-key "\C-cg" 'writegood-mode)

(provide 'global)
