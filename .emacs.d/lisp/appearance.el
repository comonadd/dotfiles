;;; Appearance --- Main Emacs appearance configuration file
;;; Commentary:
;;; Code:

;; Disable some UI unneeded stuff
(tooltip-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(global-hl-line-mode 1)
(setq use-dialog-box nil)
(set-fringe-mode 0)
(show-paren-mode t)

;; Disable splash screen
(setq inhibit-splash-screen t)

;; Font
(require 'font-lock)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(add-to-list 'default-frame-alist '(font . "Mono-10"))
(set-face-attribute 'default t :font "Mono-10")

;; Set default font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 100
                    :weight 'normal
                    :width 'normal)

(setq search-highlight t)
(setq query-replace-highlight t)

;; Bright-red TODOs and green NOTEs
(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-note-face)
(mapc (lambda (mode)
      (font-lock-add-keywords
       mode
       '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
         ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
      fixme-modes)
(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)

(provide 'appearance)
;;; appearance.el ends here
