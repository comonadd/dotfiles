;;
;;; Startup
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;;
;;; Disable annoying temporary files
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Language environment
(set-language-environment "utf-8")

;; Turn off the bell on Mac OS X
(defun nil-bell ())
(setq ring-bell-function 'nil-bell)

;; Do not require last line to be a newline
(setq require-final-newline nil)

;;
;;; Editing
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default
  indent-tabs-mode nil
  c-basic-offset 2
  tab-width 2
  python-indent-offset 4
  js-indent-level 2
  js2-basic-offset 2
  javascript-indent-level 2
  evil-shift-width 2
  css-indent-offset 2)

(setq compilation-scroll-output t)

(add-to-list 'auto-mode-alist '("\\.jpg$" . text-mode))
