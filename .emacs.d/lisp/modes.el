(require 'emmet-mode)

;; Python
(add-hook 'python-mode
  (lambda () (company-mode +1) (lsp) (turn-on-fic-ext-mode)))

;; Web-mode
(add-hook 'web-mode-hook
  (lambda ()
    ;; Enable company & LSP for JS files
    (when
      (member
        (file-name-extension buffer-file-name)
        '("tsx" "jsx" "js" "ts"))
      ;; (company-mode +1)
      (setq web-mode-markup-indent-offset 4)
      ;; (lsp)
      ;; Enable flycheck
      ;; (flycheck-mode)
      ;; (flycheck-select-checker 'javascript-eslint)
      )
    ;; Use CSS Emmet mode for stylesheet files
    (when
      (member
        (file-name-extension buffer-file-name)
        '("css" "scss" "sass" "less"))
      (emmet-mode)
      (setq emmet-use-css-transform t))
    ;; Use CSS Emmet mode for stylesheet files
    (when
      (member (file-name-extension buffer-file-name) '("html" "ejs"))
      (emmet-mode))))

;; C/C++
(add-hook 'c-mode-common-hook
  (lambda ()
    (unless
      (or
        (file-exists-p
          (concat
            (file-name-as-directory projectile-project-root)
            "makefile"))
        (file-exists-p
          (concat
            (file-name-as-directory projectile-project-root)
            "Makefile")))
      (set (make-local-variable 'compile-command) "make -k"))))

;; Makefile
(add-hook 'makefile-mode-hook '(lambda () (setq tab-width 4)))
