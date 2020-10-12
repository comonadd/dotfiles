;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.

(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Open maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Do not ask again for exit
(setq confirm-kill-emacs nil)

;; PATH Configuration
(setq exec-path
      (list (concat (getenv "HOME") "/.asdf/shims")
            "/usr/local/bin/"
            "/usr/bin/"
            "/bin/"
            "/usr/sbin/"
            "/sbin/"
            (concat (getenv "HOME") "/.cargo/bin")
            (concat (getenv "HOME") "/.local/bin")
            (concat (getenv "HOME") "/Scripts")))
(setenv "PATH" (string-join exec-path ":"))
(setq lsp-pyls-plugins-flake8-filename (concat (getenv "HOME") "/.asdf.shims"))
(setq racer-rust-src-path (concat (string-trim (shell-command-to-string "rustc --print sysroot")) "/lib/rustlib/src/rust/src"))
(setenv "RUST_SRC_PATH" racer-rust-src-path)

;; ???
(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 )
(setq gcmh-high-cons-threshold 16777216)

;; Auto-revert mode settings
(global-auto-revert-mode nil)

;; Indentation
(setq c-default-style "linux")
(setq-default indent-tabs-mode nil)
(setq c-basic-indent 4)
(setq-default tab-width 4)
(setq c-basic-offset 4)                ;; c/c++
(setq coffee-tab-width 2)              ;; coffeescript
(setq javascript-indent-level 2)       ;; javascript-mode
(setq js-indent-level 2)               ;; js-mode
(setq js2-basic-offset 2)              ;; js2-mode, in latest js2-mode, it's alias of js-indent-level
(setq-default web-mode-code-indent-offset 2)   ;; web-mode, js code in html file
(setq web-mode-markup-indent-offset 4) ;; web-mode, html tag in html file
(setq web-mode-css-indent-offset 4)    ;; web-mode, css in html file
(setq css-indent-offset 4)             ;; css-mode
(setq typescript-indent-level 2)

;; Editing
(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      inhibit-compacting-font-caches t            ; When there are lots of glyphs, keep them in memory
      truncate-string-ellipsis "…")               ; Unicode ellispis are nicer than "...", and also save /precious/ space
(setq-default truncate-lines 1)                   ; Turn off line wrapping
(delete-selection-mode 1)                         ; Replace selection when inserting text
(display-time-mode 1)                             ; Enable time in the mode-line
(global-subword-mode 1)                           ; Iterate through CamelCase words

;; Search settings
(setq search-highlight t
      search-whitespace-regexp ".*?"
      isearch-lax-whitespace t
      isearch-regexp-lax-whitespace nil
      isearch-lazy-highlight t
      isearch-lazy-count t
      lazy-count-prefix-format " (%s/%s) "
      lazy-count-suffix-format nil
      isearch-yank-on-move 'shift
      isearch-allow-scroll 'unlimited)

;; ???
(setq evil-vsplit-window-right t
      evil-split-window-below t)

;; Flyspell
;;(after! flyspell (require 'flyspell-lazy) (flyspell-lazy-mode 1)) ;; fixes slow typing

;; Projectile
(setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/"))
(defun projectile-ignored-project-function (filepath)
  "Return t if FILEPATH is within any of `projectile-ignored-projects'"
  (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))
(setq
 projectile-globally-ignored-files
 (append
  '("*.svn-base" "*.o" "*.pyc" "package-lock.json" "yarn.lock")
  projectile-globally-ignored-files))

;; Language Server
(use-package! lsp
  :init
  (setq lsp-pyls-plugins-pylint-enabled t)
  (setq lsp-pyls-plugins-autopep8-enabled nil)
  (setq lsp-pyls-plugins-yapf-enabled t)
  (setq lsp-pyls-plugins-pyflakes-enabled nil))

(use-package! lsp-mode
  :hook ((c-mode          ; clangd
          c++-mode        ; clangd
          c-or-c++-mode   ; clangd
          python-mode     ; mspyls
          js-mode         ; ts-ls (tsserver wrapper)
          js-jsx-mode     ; ts-ls (tsserver wrapper)
          typescript-mode ; ts-ls (tsserver wrapper)
          js2-mode
          typescript-tsx-mode
          javascript-tsx-mode
          web-mode
          ) . lsp)
  :commands lsp
  :init
  (setq lsp-keep-workspace-alive nil
        lsp-auto-guess-root t
        lsp-idle-delay 0.5
        lsp-client-packages nil
        lsp-eldoc-hook nil
        )
  :config
  (advice-add #'lsp--auto-configure :override #'ignore)
  ;;(add-to-list 'lsp-language-id-configuration '(js-jsx-mode . "javascriptreact"))
  )
(use-package! lsp-ui
  :commands lsp-ui-mode)


;; (use-package lsp-python-ms
;;   :ensure t
;;   :init (setq lsp-python-ms-auto-install-server t)
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-python-ms)
;;                           (lsp))))

;; company
;; (use-package company
;;   :config
;;   (setq company-backends
;;     '(company-files          ; files & directory
;;        company-keywords       ; keywords
;;        company-yasnippet
;;        company-capf
;;      ))
;;   (setq company-idle-delay 0.5
;;         company-minimum-prefix-length 2)
;;   (setq company-show-numbers t)
;;   (setq company-selection-wrap-around t)
;;   (setq company-tooltip-align-annotations t)
;;   (setq company-frontends '(company-box-frontend))
;;   (with-eval-after-load 'company
;;     (define-key company-active-map (kbd "C-j") nil) ; avoid conflict with emmet-mode
;;     (define-key company-active-map (kbd "C-n") #'company-select-next)
;;     (define-key company-active-map (kbd "C-p") #'company-select-previous)))
;; (setq-default history-length 1000)
;; (setq-default prescient-history-length 1000)
;; (use-package! company-lsp
;;   :commands company-lsp)
;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

;; Tide
;; (defun setup-tide-mode ()
;;  (interactive)
;;  (tide-setup)
;;  (flycheck-mode +1)
;;  (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;  (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;  (company-mode +1))

;; Flycheck
(use-package flycheck
  :hook ((js2-mode . flycheck-mode))
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled newline))
  (setq flycheck-display-errors-delay 0.1)
  (setq flycheck-flake8rc "~/.config/flake8"))

;; jsx
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
;;         ("\\.jsx?\\'"  . web-mode)
;;         ("\\.tsx?\\'"  . web-mode)
         ("\\.json\\'"  . web-mode))
  :config
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  ; (web-mode-set-engine nil)
  ; (+web-django-mode nil)
  )
(setq web-mode-engines-alist
      '(
        ("none"   . "\\.js\\.")
        ("none"   . "\\.ts\\'")
        ("none"   . "\\.jsx\\.")
        ("none"   . "\\.tsx\\'")
       )
)

;; projectile
(use-package projectile
  :config
  (setq projectile-sort-order 'recentf)
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-completion-system 'ivy)
  (setq projectile-mode-line-prefix " ")
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map)
  (define-key projectile-mode-map (kbd "s-p") #'projectile-find-file)
  (define-key projectile-mode-map (kbd "C-p") #'projectile-find-file)
  (define-key projectile-mode-map (kbd "s-F") #'projectile-ripgrep)
  (define-key projectile-mode-map (kbd "C-S-f") #'projectile-ripgrep))

;; Javascript
(defun setup-js-indentation ()
  (setq tab-width 2)
  (setq c-basic-indent 2)
  (setq tab-width 2)
  (setq c-basic-offset 2)
  (setq-default typescript-indent-level 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-content-type "jsx"))
(defun setup-prettier-js ()
  (prettier-js-mode)
  (remove-hook 'before-save-hook 'prettier-js t))
(dolist (hook '(
                typescript-tsx-mode-hook
                javascript-tsx-mode-hook
                js2-mode-hook
                typescript-mode-hook
                web-mode-hook
                ))
  (add-hook hook (lambda ()
                   (setup-js-indentation)
                   (setup-prettier-js)
                   (disable-smartparens)
                   ;; (setq company-backends
                   ;;       '(
                   ;;         company-keywords       ; keywords
                   ;;         company-files          ; files & directory
                   ;;         company-capf           ; lsp
                   ;;         ))
                   ;;(setup-tide-mode)
                   ;; (after! company-lsp
                   ;;   (add-to-list (make-local-variable 'company-backends) 'company-lsp)
                   ;;   )
                   ))
  )
(add-hook! 'web-mode-hook
  (setup-prettier-js)
  (disable-smartparens))

;; Python
(add-hook! 'python-mode-hook
  (disable-smartparens)
  (lsp-workspace-folders-add (projectile-project-root))
  (setq flycheck-python-pylint-executable (concat (getenv "HOME") "/.asdf/shims/pylint"))
  (setq flycheck-pylintrc (concat (projectile-project-root) ".pylintrc")))

;; Rust
(add-hook! 'rustic-mode-hook
  (disable-smartparens)
  (setq lsp-rust-server 'rust-analyzer)
  (setq rustic-lsp-server 'rust-analyzer))

;; Org
(setq org-directory "~/org/")

;; Load submodules
(load! "functions.el")
(load! "theming.el")
(load! "bindings.el")
(load! "commands.el")
