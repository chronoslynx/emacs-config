(use-package hydra)
;; Yank-pop
(defhydra hydra-yank-pop ()
  "yank"
  ("C-y" yank nil)
  ("M-y" yank-pop nil)
  ("y" (yank-pop 1) "next")
  ("Y" (yank-pop -1) "prev")
  ("l" helm-show-kill-ring "list" :color blue))   ; or browse-kill-ring
(global-set-key (kbd "M-y") #'hydra-yank-pop/yank-pop)
(global-set-key (kbd "C-y") #'hydra-yank-pop/yank)

;; Flycheck controls
(defhydra hydra-flycheck (global-map "C-c e")
  "Compilation errors"
  ("h" flycheck-first-error)
  ("j" flycheck-next-error)
  ("k" flycheck-previous-error)
  ("l" (condition-case err
           (while t
             (flycheck-next-error))
         (user-error nil))
   nil :bind nil)
  ("q" nil            nil :color blue) )

;; Org-mode helpers
(defhydra hydra-org ()
  "
Org-controls
---------------------------
_c_: capture  _a_: agenda
_r_: refile   _c_: copy
_v_: archive
"
  ("c" org-capture)
  ("a" org-agenda)
  ("r" org-refile)
  ("y" org-copy)
  ("v" org-archive-subtree-default)
  )

(defhydra hydra-windows (:exit t)
  "
^Focus^              ^Management^
---------------------------------------
_o_: other-window    _0_: delete-window
_h_: left            _1_: delete-other
_l_: right           _2_: vertical
_k_: up              _3_: horizontal
_j_: down            _f_: delete-frame
"
  ("o" other-window)
  ("h" windmove-left)
  ("l" windmove-right)
  ("j" windmove-down)
  ("k" windmove-up)
  ("0" delete-window :color blue)
  ("1" delete-other-windows)
  ("2" split-window-vertically)
  ("3" split-window-horizontally)
  ("f" delete-frame)
  )

(defhydra hydra-describe ()
  "
Describe:
_k_: key-binding
_f_: function
_v_: variable
_m_: mode
"
  ("k" describe-key)
  ("f" counsel-describe-function)
  ("v" counsel-describe-variable)
  ("m" describe-mode))

;; Buffer controls
(defhydra hydra-buffer-menu (:color pink
                                    :hint nil)
  "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
  ("m" Buffer-menu-mark)
  ("u" Buffer-menu-unmark)
  ("U" Buffer-menu-backup-unmark)
  ("d" Buffer-menu-delete)
  ("D" Buffer-menu-delete-backwards)
  ("s" Buffer-menu-save)
  ("~" Buffer-menu-not-modified)
  ("x" Buffer-menu-execute)
  ("b" Buffer-menu-bury)
  ("g" revert-buffer)
  ("T" Buffer-menu-toggle-files-only)
  ("O" Buffer-menu-multi-occur :color blue)
  ("I" Buffer-menu-isearch-buffers :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))

(define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)

;; Projectile
(defhydra hydra-projectile-other-window (:color teal)
  "projectile-other-window"
  ("f"  projectile-find-file-other-window        "file")
  ("g"  projectile-find-file-dwim-other-window   "file dwim")
  ("d"  projectile-find-dir-other-window         "dir")
  ("b"  projectile-switch-to-buffer-other-window "buffer")
  ("q"  nil                                      "cancel" :color blue))

(defhydra hydra-projectile (:color teal
                                   :hint nil)
  "
     PROJECTILE: %(projectile-project-root)

     Find...                 Search                 Buffers                 Management
---------------------------------------------------------------------------------
  _f_: file               _s_: ag                _i_: Ibuffer            _x_: remove known project
 _ed_: file curr dir      _m_: multi-occur       _b_: switch to buffer   _X_: cleanup known projects
  _s-k_: Kill all buffers
  _d_: dir                _`_: other-window

"
  ("s"   projectile-find-file)
  ("b"   counsel-projectile-switch-to-buffer)
  ("d"   counsel-projectile-find-dir)
  ("f"   counsel-projectile-find-file)
  ("ed"  projectile-find-file-in-directory)
  ("i"   projectile-ibuffer)
  ("s-k" projectile-kill-buffers)
  ("m"   projectile-multi-occur)
  ("p"   counsel-projectile)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("`"   hydra-projectile-other-window/body "other window")
  ("q"   nil "cancel" :color blue))

;; Markdown-mode
(defhydra dh-hydra-markdown-mode (:hint nil)
  "
Formatting        C-c C-s    _s_: bold          _e_: italic     _b_: blockquote   _p_: pre-formatted    _c_: code

Headings          C-c C-t    _h_: automatic     _1_: h1         _2_: h2           _3_: h3               _4_: h4

Lists             C-c C-x    _m_: insert item

Demote/Promote    C-c C-x    _l_: promote       _r_: demote     _u_: move up      _d_: move down

Links, footnotes  C-c C-a    _L_: link          _U_: uri        _F_: footnote     _W_: wiki-link      _R_: reference

"

  ("s" markdown-insert-bold)
  ("e" markdown-insert-italic)
  ("b" markdown-insert-blockquote :color blue)
  ("p" markdown-insert-pre :color blue)
  ("c" markdown-insert-code)

  ("h" markdown-insert-header-dwim)
  ("1" markdown-insert-header-atx-1)
  ("2" markdown-insert-header-atx-2)
  ("3" markdown-insert-header-atx-3)
  ("4" markdown-insert-header-atx-4)

  ("m" markdown-insert-list-item)

  ("l" markdown-promote)
  ("r" markdown-demote)
  ("d" markdown-move-down)
  ("u" markdown-move-up)

  ("L" markdown-insert-link :color blue)
  ("U" markdown-insert-uri :color blue)
  ("F" markdown-insert-footnote :color blue)
  ("W" markdown-insert-wiki-link :color blue)
  ("R" markdown-insert-reference-link-dwim :color blue)
  )

(global-set-key [f9] 'dh-hydra-markdown-mode/body)

(defhydra hydra-gtags ()
  "
  _s_: find symbol       _d_: find definition
  _f_: find file
  _c_: create tags
  _r_: find reference    _u_: update tags
"
  ("s" ggtags-find-other-symbol)
  ("f" ggtags-find-file)
  ("x" ggtags-view-tag-history)
  ("r" ggtags-find-reference)
  ("d" ggtags-find-definition)
  ("c" ggtags-create-tags)
  ("u" ggtags-update-tags)
  )

(provide 'my-hydras)
