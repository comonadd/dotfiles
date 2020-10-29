;;; Global keybindings not related to any of the packages installed.
;;; Package-specific keybindings reside in packages.el.

;; Basics
(evil-set-leader 'normal (kbd "SPC"))
(evil-define-key 'visual 'global (kbd "<leader>mer") 'eval-region)
(evil-define-key 'normal 'global (kbd "<leader>meb") 'eval-buffer)
(evil-define-key 'normal 'global (kbd "<leader>ee") 'open-config)
(evil-define-key 'normal 'global (kbd "<leader>er") 'reload-config)
(evil-define-key 'normal 'global (kbd "<leader>qq") 'kill-emacs)
(if (eq system-type 'windows-nt)
    (evil-define-key 'normal 'global (kbd "<M-f4>") 'kill-emacs))
(evil-define-key 'normal 'global (kbd "<leader>qr") 'restart-emacs)

;; Help
(evil-define-key 'normal 'global (kbd "<leader>hv") 'describe-variable)
(evil-define-key 'normal 'global (kbd "<leader>hf") 'describe-function)
(evil-define-key 'normal 'global (kbd "<leader>hb") 'describe-key)

;; Files
(evil-define-key 'normal 'global (kbd "<leader>ff") 'find-file)
(evil-define-key 'normal 'global (kbd "<leader>fs") 'save-buffer)
(evil-define-key 'normal 'global (kbd "<leader>fa") 'save-all)

;; Windows
(evil-define-key 'normal 'global (kbd "<leader><tab>")
  (lambda ()
    (interactive)
    (my/switch-to-previous-buffer)))
(evil-define-key 'normal 'global (kbd "<leader>wh") 'windmove-left)
(evil-define-key  'normal 'global (kbd "<leader>wl") 'windmove-right)
(evil-define-key 'normal 'global (kbd "<leader>wj") 'windmove-down)
(evil-define-key 'normal 'global (kbd "<leader>wk") 'windmove-up)
(evil-define-key 'normal 'global (kbd "<leader>wv") 'evil-window-vsplit)
(evil-define-key 'normal 'global (kbd "<leader>ws") 'evil-window-split)
(evil-define-key 'normal 'global (kbd "<leader>wd") 'delete-window)

;; File navigation
(evil-define-key 'normal 'global (kbd "<leader>/") 'swiper)

;; Editing
(global-set-key (kbd "M-d") 'kill-whole-line)

;; Navigation
(evil-define-key 'normal 'global (kbd "C-b") 'evil-goto-definition)

;; Formatting
(when (eq system-type 'windows-nt)
  (global-set-key (kbd "<M-lwindow> M-l") 'lsp-format-buffer)
  (define-key emacs-lisp-mode-map (kbd "<M-lwindow> M-l") 'elisp-format-buffer)
  (require 'cc-mode)
  (define-key c-mode-base-map (kbd "<M-lwindow> M-l") 'clang-format-buffer))

;; buffers
(evil-define-key 'normal 'global (kbd "<leader>bb") 'switch-to-buffer)
(evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-buffer)