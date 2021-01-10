;; ELisp
(add-hook 'emacs-lisp-mode (lambda ()
                             (hl-todo-mode)
                             (local-set-key (kbd "M-d") 'kill-whole-line)))

;; Python
(add-hook 'python-mode (lambda ()
                         (hl-todo-mode)
                         (company-mode +1)
                         (lsp)))

;; Web-mode
(add-hook 'web-mode-hook (lambda ()
                           (hl-todo-mode)
                           (when (member (file-name-extension buffer-file-name)
                                         '("tsx" "jsx" "js" "ts"))
                             (company-mode +1)
                             (lsp))))

;; C/C++
(add-hook 'c-mode-common-hook '(lambda ()
                                 (hl-todo-mode)
                                 (unless (or (file-exists-p (concat (file-name-as-directory
                                                                     projectile-project-root)
                                                                    "makefile"))
                                             (file-exists-p (concat (file-name-as-directory
                                                                     projectile-project-root)
                                                                    "Makefile")))
                                   (set (make-local-variable 'compile-command) "make -k"))))

;; Makefile
(add-hook 'makefile-mode-hook '(lambda ()
                                 (setq tab-width 4)))
