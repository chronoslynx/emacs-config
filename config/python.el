(require 'jedi)
(require 'sphinx-doc)
;; (eval-after-load 'company '(push 'company-jedi company-backends))
(use-package 'python-mode
  :config
  (use-package jedi)
  (use-package sphinx-doc)
  (sphinx-doc-mode t)
  (add-to-list 'company-backends 'company-jedi)
  (jedi:setup)
  (add-to-list 'interpreter-mode-alist '("python2" . python-mode))
  (add-to-list 'interpreter-mode-alist '("python3" . python-mode)))

(provide 'python)
