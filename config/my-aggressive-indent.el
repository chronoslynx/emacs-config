(defvar my:aggressive-modes
  '("python"
    "rust"
    "go"
    )
  "Modes to use aggressive indentation in"
  )

(mapc
 (lambda (mode)
   (add-hook
    (intern (concat mode "-mode-hook"))
    #'aggressive-indent-mode))
 my:aggressive-modes)

(provide 'my-aggressive-indent)
