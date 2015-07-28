(require 'jedi)

(add-hook 'python-mode-hook 'jedi:setup)
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)
(provide 'python)
