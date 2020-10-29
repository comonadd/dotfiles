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

;;
;;; Basics

;; Personal information
(setq user-full-name "Dmitry Guzeev"
      user-mail-address "dmitri.guzeev@gmail.com")

;; Open maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq-default
 confirm-kill-emacs nil                           ; Do not ask again for exit
 gcmh-high-cons-threshold 16777216                ; ???
 delete-by-moving-to-trash nil                    ; Do not delete files to trash
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 )

;; PATH
(if (eq system-type 'darwin)
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
  (setq lsp-pyls-plugins-flake8-filename (concat (getenv "HOME") "/.asdf.shims"))
  (setq racer-rust-src-path (concat (string-trim (shell-command-to-string "rustc --print sysroot")) "/lib/rustlib/src/rust/src"))
  (setq flycheck-python-pylint-executable
        (expand-file-name (concat (file-name-as-directory (getenv "HOME")) ".asdf/shims/pylint")))
  )
(if (eq system-type 'windows-nt)
    (setq exec-path
          (append
           exec-path
           (list
            "C:\\Windows\\System32"
            ;; "C:\\Program Files\\Git\\bin"
            ;; "C:\\ProgramData\\chocolatey\\bin"
            ;; (concat (getenv "HOME") ".pyenv\\pyenv-win\\shims")
            )
           ))
  (setq-default flycheck-python-pylint-executable
                (concat (file-name-as-directory (getenv "HOME")) ".pyenv\\pyenv-win\\shims\\pylint"))
  )
(setenv "PATH" (string-join exec-path ":"))

;; Auto-revert mode settings
(global-auto-revert-mode nil)

;; Indentation
(setq-default
 c-default-style "linux"
 indent-tabs-mode nil
 c-basic-indent 4
 tab-width 4
 c-basic-offset 4                ; c/C++
 coffee-tab-width 2              ; coffeescript-mode
 javascript-indent-level 2       ; javascript-mode
 js-indent-level 2               ; js-mode
 js2-basic-offset 2              ; js2-mode
 web-mode-code-indent-offset 2   ; web-mode, js code in html file
 web-mode-markup-indent-offset 4 ; web-mode, html tag in html file
 web-mode-css-indent-offset 4    ; web-mode, css in html file
 css-indent-offset 4             ; css-mode
 typescript-indent-level 2       ; typescript
 )

;; Editing
(setq-default undo-limit 80000000                         ; Raise undo-limit to 80Mb
              evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
              auto-save-default t                         ; Nobody likes to loose work, I certainly don't
              inhibit-compacting-font-caches t            ; When there are lots of glyphs, keep them in memory
              truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
              truncate-lines 1                            ; Turn off line wrapping
              )
(delete-selection-mode 1)                                 ; Replace selection when inserting text
(display-time-mode 1)                                     ; Enable time in the mode-line
(global-subword-mode 1)                                   ; Iterate through CamelCase words

;; Search
(setq-default search-highlight t
              search-whitespace-regexp ".*?"
              isearch-lax-whitespace t
              isearch-regexp-lax-whitespace nil
              isearch-lazy-highlight t
              isearch-lazy-count t
              lazy-count-prefix-format " (%s/%s) "
              lazy-count-suffix-format nil
              isearch-yank-on-move 'shift
              isearch-allow-scroll 'unlimited)

;; Evil
(setq evil-vsplit-window-right t
      evil-split-window-below t)

;;
;;; Tools

;; Projectile
(setq-default
 projectile-project-search-path (concat (file-name-as-directory (getenv "HOME")) "Projects")
 projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/")
 projectile-globally-ignored-files (append '("*.svn-base" "*.o" "*.pyc" "package-lock.json" "yarn.lock") projectile-globally-ignored-files)
 )
(defun projectile-ignored-project-function (filepath)
  "Return t if FILEPATH is within any of `projectile-ignored-projects'"
  (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))

;; Language server protocol
(use-package! lsp
  :init
  (setq-default
   ;; PyLS
   lsp-pyls-plugins-pylint-enabled t
   lsp-pyls-plugins-autopep8-enabled nil
   lsp-pyls-plugins-yapf-enabled t
   lsp-pyls-plugins-pyflakes-enabled nil
   )
  )
(use-package! lsp-mode
  :hook ((c-mode
          c++-mode
          c-or-c++-mode
          python-mode
          js-mode
          js-jsx-mode
          typescript-mode
          js2-mode
          typescript-tsx-mode
          javascript-tsx-mode
          web-mode
          ) . lsp)
  :commands lsp
  :init
  (setq
   lsp-keep-workspace-alive nil    ; Close workspace after the last buffer is closed
   lsp-auto-guess-root t           ; Automatically guess the project root using projectile
   lsp-eldoc-hook nil              ; Disable annoying eldoc annotations
   lsp-client-packages nil
   lsp-idle-delay 0.25
   )
  :config
  (advice-add #'lsp--auto-configure :override #'ignore)
  ;;(add-to-list 'lsp-language-id-configuration '(js-jsx-mode . "javascriptreact"))
  )
(use-package! lsp-ui
  :commands lsp-ui-mode)

;; company
(use-package company
  :config
  (setq
   company-idle-delay nil                  ; Do not show suggestions automatically
   company-show-numbers t
   company-minimum-prefix-length 2
   company-selection-wrap-around t
   company-tooltip-align-annotations t
   )
  )
;;
;;   (setq company-backends
;;     '(company-files          ; files & directory
;;        company-keywords       ; keywords
;;        company-yasnippet
;;        company-capf
;;      ))
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
(global-flycheck-mode -1)
;; (use-package flycheck
;;   :hook ((js2-mode . flycheck-mode))
;;   :init
;;   :config
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled newline))
;;   (setq flycheck-display-errors-delay 0.1)
;;   (setq flycheck-flake8rc (concat (file-name-as-directory (getenv "HOME")) ".config/flake8")))

;; jsx
(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'"   . web-mode)
;;         ("\\.jsx?\\'"  . web-mode)
;;         ("\\.tsx?\\'"  . web-mode)
;;         ("\\.json\\'"  . web-mode)
         )
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

  ;; Setup fd
  (if (eq system-type 'windows-nt)
      (setq doom-projectile-fd-binary "C:\\ProgramData\\chocolatey\\bin\\fd.exe"))

  (projectile-mode +1)
  ;; Keybindings
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
                   (setq comment-line-break-function #'js2-line-break)
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
;; REVIEW We associate TSX files with `typescript-tsx-mode' derived from
;;        `web-mode' because `typescript-mode' does not officially support
;;        JSX/TSX. See
;;        https://github.com/emacs-typescript/typescript.el/issues/4
(if (featurep! :lang web)
    (progn
      (define-derived-mode typescript-tsx-mode web-mode "TypeScript-tsx")
      (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
      (after! flycheck
        (flycheck-add-mode 'typescript-tslint 'typescript-tsx-mode)
        (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode)))

;; Python
(add-hook! 'python-mode-hook
  (disable-smartparens)
  (lsp-workspace-folders-add (projectile-project-root))
  (setq flycheck-pylintrc
        (expand-file-name (concat (projectile-project-root) ".pylintrc")))
  )

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
