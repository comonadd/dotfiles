;;; disable-backup.el --- disable the Emacs backup files generation
;;; Commentary:
;;; Code:

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-name nil)

(provide 'disable-backup)
;;; disable-backup.el ends here
