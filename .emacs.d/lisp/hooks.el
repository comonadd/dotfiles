;;; Hooks --- Main Emacs hooks configuration file
;;; Commentary:
;;;   This file defines all the hooks
;;; Code:

(defun main-c-mode-hook ()
    "Main C Mode hook."

    (defun insert-file-top-comment ()
	"Insert the top comment to the file."
	(interactive)
	;; /* File: some.h */
	;; /* Creation date: day.month.year */
	;; /* Creator: Dmitry Guzeev <dmitry.guzeev@yahoo.com> */
	;; /* Description: */
	(setq base-file-name (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
	(insert "/* File: ")
	(insert (file-name-nondirectory buffer-file-name))
	(insert " */\n")
	(insert "/* Creation Date: ")
	(insert-current-date)
	(insert " */\n")
	(insert "/* Creator: Dmitry Guzeev <dmitry.guzeev@yahoo.com> */\n")
	(insert "/* Description: */\n"))

    (defun header-format ()
	"Format the given file as a header file."
	;; file comment
	;;
	;; #ifndef SOME_H
	;; #define SOME_H
	;;
	;;
	;;
	;; #endif // SOME_H
	(interactive)
	(setq base-file-name (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
	(insert-file-top-comment)
	(insert "\n")
	(insert "#ifndef ")
	(insert (upcase base-file-name))
	(insert "_H\n")
	(insert "#define ")
	(insert (upcase base-file-name))
	(insert "_H\n")
	(insert "\n")
	(insert "\n")
	(insert "\n")
	(insert "#endif /* ")
	(insert (upcase base-file-name))
	(insert "_H */")
	(save-buffer))

    (defun source-format ()
	"Format the given file as a source file."
	;; file comment
	;;
	;; #include "some.h"
	(interactive)
	(setq
	  base-file-name
	  (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
	(insert-file-top-comment)
	(insert "\n")
	(defun insert-include-header ()
	    (insert "#include \"")
	    (insert (concat base-file-name ".h"))
	    (insert "\"\n"))
	(if (file-exists-p (concat base-file-name ".h"))
	    (insert-include-header))
	(save-buffer))

    (cond ((file-exists-p buffer-file-name) t)
	  ((string-match "[.]c" buffer-file-name) (source-format))
	  ((string-match "[.]h" buffer-file-name) (header-format)))

    (defun find-corresponding-file ()
	"Find the file that corresponds to this one."
	(interactive)
	(setq corresponding-file-name nil)
	(setq base-file-name (file-name-sans-extension buffer-file-name))
	(if (string-match "\\.c" buffer-file-name)
	    (setq corresponding-file-name (concat base-file-name ".h")))
	(if (string-match "\\.h" buffer-file-name)
	    (if (file-exists-p (concat base-file-name ".c")) (setq corresponding-file-name (concat base-file-name ".c"))
		(setq corresponding-file-name (concat base-file-name ".c"))))
	(if corresponding-file-name (find-file corresponding-file-name)
	    (error "Unable to find a corresponding file")))

    (defun find-corresponding-file-other-window ()
	"Find the file that corresponds to this one."
	(interactive)
	(find-file-other-window buffer-file-name)
	(find-corresponding-file)
	(other-window -1))

    (defun setup-flycheck-project-path ()
	(add-to-list 'flycheck-gcc-include-path (projectile-project-root))
	(add-to-list 'flycheck-gcc-include-path (concat (projectile-project-root) "/src"))
	(add-to-list 'flycheck-gcc-include-path (concat (projectile-project-root) "/include")))

    (setq
      c-default-style "linux"
      c-basic-offset 4)
    (c-set-offset 'comment-intro 0)
    (c-set-offset 'case-label '+)
    (projectile-mode 1)
    (setup-flycheck-project-path)
    (electric-indent-mode 1)
    (ggtags-mode 1)
    (helm-gtags-mode 1)
    (define-key c-mode-map (kbd "<f12>") 'find-corresponding-file)
    (define-key c-mode-map (kbd "S-<f12>") 'find-corresponding-file-other-window))

(defun main-python-mode-hook ()
  (electric-indent-mode -1))

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'c-mode-hook 'main-c-mode-hook)
(add-hook 'c++-mode-hook 'main-c-mode-hook)
(add-hook 'python-mode-hook 'main-python-mode-hook)

(provide 'hooks)
;;; hooks.el ends here
