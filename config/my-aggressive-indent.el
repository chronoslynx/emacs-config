(require 'aggressive-indent)
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
 my:unaggressive-modes)



(provide 'my-aggressive-indent)
