;;; my/hooks --- Package that defines all the hooks
;;; Commentary:
;;; Code:

;; C mode hook
(require 'my/c-mode/main)
(add-hook 'c-mode-hook 'my/c-mode/hook)

;; C++ mode hook
(require 'my/cpp-mode/main)
(add-hook 'c++-mode-hook 'my/cpp-mode/hook)

;; Python mode hook
(require 'my/python-mode/main)
(add-hook 'python-mode-hook 'my/python-mode/hook)

;; Lisp mode hook
(require 'my/lisp-mode/main)
(add-hook 'lisp-mode-hook 'my/lisp-mode/hook)
(add-hook 'emacs-lisp-mode-hook 'my/lisp-mode/hook)

;; HTML mode hook
(require 'my/html-mode/main)
(add-hook 'html-mode-hook 'my/html-mode/hook)

;; Rust mode hook
(require 'my/rust-mode/main)
(add-hook 'rust-mode-hook 'my/rust-mode/hook)

;; CMake mode hook
(require 'my/cmake-mode/main)
(add-hook 'cmake-mode-hook 'my/cmake-mode/hook)

;; JavaScript mode hook
(require 'my/js-mode/main)
(require 'jsx-mode)
(add-hook 'js-mode-hook 'my/js-mode/hook)
(add-hook 'jsx-mode-hook 'my/js-mode/hook)

;; Other hooks
(require 'my/before-save-hook)
(add-hook 'before-save-hook 'my/before-save-hook)

(provide 'my/hooks)
;;; hooks.el ends here
