;;; hooks --- Main Emacs hooks configuration file
;;; Commentary:
;;;   This file defines all the hooks
;;; Code:

;; All the hooks related to the C
(require 'my-c-mode-hook)
(add-hook 'c-mode-hook 'my/c-mode-hook)

;; All the hooks related to the C++
(require 'my-cpp-mode-hook)
(add-hook 'c++-mode-hook 'my/c++-mode-hook)

;; All the hooks related to the Python
(require 'my-python-mode-hook)
(add-hook 'python-mode-hook 'my/python-mode-hook)

;; All the hooks related to the Lisp
(require 'my-lisp-mode-hook)
(add-hook 'lisp-mode-hook 'my/lisp-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my/lisp-mode-hook)

;; All the hooks related to the HTML mode
(require 'my-html-mode-hook)
(add-hook 'html-mode-hook 'my/html-mode-hook)

;; All the hooks related to the Rust mode
(require 'my-rust-mode-hook)
(add-hook 'rust-mode-hook 'my/rust-mode-hook)

;; Cmake mode hook
(require 'my-cmake-mode-hook)
(add-hook 'cmake-mode-hook 'my/cmake-mode-hook)

;; JavaScript mode hook
(require 'my-js-mode-hook)
(add-hook 'js-mode-hook 'my/js-mode-hook)

;; Other hooks
(require 'my-before-save-hook)
(add-hook 'before-save-hook 'my/before-save-hook)

(provide 'hooks)
;;; hooks.el ends here
