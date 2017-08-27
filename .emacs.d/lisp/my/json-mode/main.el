;;; main.el --- JSON mode stuff
;;; Commentary:
;;; Code:

(require 'js2-mode)

(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

(provide 'my/json-mode/main)
;;; main.el ends here
