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
;; (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;   (setup-tide-mode))

;; JavaScript
;; (add-hook 'js2-mode (lambda ()
;; 			 (company-mode +1)))
