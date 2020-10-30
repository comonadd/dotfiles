;; ELisp
(add-hook 'emacs-lisp-mode (lambda ()
                             (local-set-key (kbd "M-d") 'kill-whole-line)))

;; Python
(add-hook 'python-mode (lambda ()
                         (company-mode +1)
                         (lsp)))

;; Web-mode
(add-hook 'web-mode-hook (lambda ()
                           (when (member (file-name-extension buffer-file-name)
                                         '("tsx" "jsx" "js" "ts"))
                             (company-mode +1)
                             (lsp))))
