;;; Global keybindings not related to any of the packages installed.
;;; Package-specific keybindings reside in packages.el.

;; Basics
(evil-set-leader 'normal (kbd "SPC"))
(global-set-key (kbd "<f1>") 'eval-region)
(evil-define-key 'normal 'global (kbd "<leader>meb") 'eval-buffer)
(evil-define-key 'normal 'global (kbd "<leader>ee") 'open-config)
(evil-define-key 'normal 'global (kbd "<leader>er") 'reload-config)
(evil-define-key 'normal 'global (kbd "<leader>qq") 'kill-emacs)
(if (eq system-type 'windows-nt)
  (evil-define-key 'normal 'global (kbd "<M-f4>") 'kill-emacs))
(evil-define-key 'normal 'global (kbd "<leader>qr") 'restart-emacs)
;; no screwing with my middle mouse button
(global-unset-key [mouse-2])
(require 'face-remap)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<f8>") 'my/read-dir-locals)

(evil-define-key 'normal 'global (kbd "u") 'undo)
(global-set-key (kbd "M-r") 'undo-redo)

;; Help
(evil-define-key
  'normal
  'global
  (kbd "<leader>hv")
  'describe-variable)
(evil-define-key
  'normal
  'global
  (kbd "<leader>hf")
  'describe-function)
(evil-define-key 'normal 'global (kbd "<leader>hb") 'describe-key)

;; Files
(evil-define-key 'normal 'global (kbd "<leader>ff") 'find-file)
(evil-define-key
  'normal
  'global
  (kbd "<leader>fo")
  'find-file-other-window)
(evil-define-key 'normal 'global (kbd "<leader>fs") 'save-buffer)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-S-s") 'save-all)
(evil-define-key 'normal 'global (kbd "<leader>fa") 'save-all)
(evil-define-key 'normal 'global (kbd "M-s") 'my/switch-to-alt-file)
(evil-define-key
  'normal
  'global
  (kbd "<leader>fr")
  'my/rename-current-file)
(evil-define-key
  'normal
  'global
  (kbd "<leader>fd")
  'my/delete-current-file)

;; Windows
(evil-define-key
  'normal
  'global
  (kbd "<leader><tab>")
  'evil-switch-to-windows-last-buffer)

(global-set-key (kbd "C-h") 'evil-window-left)
(global-set-key (kbd "C-l") 'evil-window-right)
(global-set-key (kbd "C-j") 'evil-window-down)
(global-set-key (kbd "C-k") 'evil-window-up)

(evil-define-key
  'normal
  'global
  (kbd "<leader>wv")
  'evil-window-vsplit)
(evil-define-key
  'normal
  'global
  (kbd "<leader>ws")
  'evil-window-split)
(evil-define-key 'normal 'global (kbd "<leader>wd") 'delete-window)

;; File navigation
(evil-define-key 'normal 'global (kbd "/") 'swiper-isearch)
(evil-define-key 'normal 'global (kbd "<leader>p/") 'projectile-ag)
(global-set-key (kbd "M-n") 'flycheck-next-error)
(global-set-key (kbd "M-p") 'flycheck-previous-error)

;; Editing
(global-set-key (kbd "M-d") 'kill-whole-line)
(global-set-key (kbd "C-c C-u") 'string-inflection-all-cycle)
(global-set-key (kbd "C-c C-k") 'string-inflection-kebab-case)

;; Navigation
(evil-define-key 'normal 'global (kbd "C-b") 'evil-goto-definition)
(evil-define-key 'normal 'global (kbd "C-j") 'imenu)
(evil-define-key 'normal 'global (kbd "C-r") 'query-replace)
(evil-define-key 'normal 'global (kbd "n") 'evil-search-previous) ;; Switch the two operations back to normal since swiper integration somehow makes it backwards
(evil-define-key 'normal 'global (kbd "N") 'evil-search-next)

(global-unset-key (kbd "M-l"))
(global-unset-key (kbd "M-h"))

;; Formatting
(when (eq system-type 'windows-nt)
  (global-set-key (kbd "<M-lwindow> M-l") 'lsp-format-buffer)
  (define-key emacs-lisp-mode-map (kbd "<M-lwindow> M-l")
    'elisp-autofmt-buffer)
  (require 'cc-mode)
  (define-key c-mode-base-map (kbd "<M-lwindow> M-l")
    'clang-format-buffer))

;; buffers
(evil-define-key 'normal 'global (kbd "<leader>bb") 'switch-to-buffer)
(evil-define-key 'normal 'global (kbd "M-b") 'switch-to-buffer)
(evil-define-key 'normal 'global (kbd "<leader>bk") 'kill-buffer)
(evil-define-key 'normal 'global (kbd "M-k") 'kill-current-buffer)
(global-set-key (kbd "M-o") 'my/view-current-file-other-window)

;; Project
(evil-define-key
  'normal
  'global
  (kbd "<f6>")
  'projectile-compile-project)

;; Configuration
(evil-define-key 'normal 'global (kbd "<leader>tt") 'load-theme)
(evil-define-key 'normal 'global (kbd "<leader>tt") 'my/reset-themes)
(evil-define-key 'normal 'global (kbd "C-<f8>") 'my/next-theme)
