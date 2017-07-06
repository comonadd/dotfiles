;;; my/modes --- Package that setups other modes
;;; Commentary:
;;; Code:

;; Modes
(global-subword-mode)
(require 'my/c-mode/main)
(require 'my/cpp-mode/main)
(require 'my/html-mode/main)
(require 'my/rust-mode/main)
(require 'my/lisp-mode/main)
(require 'my/cmake-mode/main)
(require 'my/python-mode/main)
(require 'my/js-mode/main)
(require 'my/css-mode/main)
(require 'my/ebnf-mode)

(provide 'my/modes)
;;; modes.el ends here
