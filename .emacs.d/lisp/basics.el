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
  javascript-indent-level 2)
