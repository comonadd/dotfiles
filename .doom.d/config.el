;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;

;; Open maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized)) ; Open maximized

;; Backup
;(setq backup-directory-alist `(("." . "~/.emacs-tmp/")))
;(setq auto-save-file-name-transforms `((".*" "~/.emacs-tmp/" t)))

;; Do not ask again for exit
(setq confirm-kill-emacs nil)

;; Indentation
(setq c-default-style "linux")
(setq-default indent-tabs-mode nil)
(setq c-basic-indent 4)
(setq tab-width 4)
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

;; ???
(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 tab-width 4                                      ; Set width for tabs
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width
(setq gcmh-high-cons-threshold 16777216)

;; Auto-revert mode settings
(global-auto-revert-mode nil)

;; Whitespaces
; (setq
;  whitespace-line-column 100
;  whitespace-style
;  '(face trailing lines-tail))
; (global-whitespace-mode)
;(add-hook 'before-save-hook 'whitespace-cleanup)

;; Turn off line wrapping
(setq-default truncate-lines 1)

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

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      inhibit-compacting-font-caches t            ; When there are lots of glyphs, keep them in memory
      truncate-string-ellipsis "…")               ; Unicode ellispis are nicer than "...", and also save /precious/ space

(delete-selection-mode 1)                         ; Replace selection when inserting text
(display-time-mode 1)                             ; Enable time in the mode-line
(global-subword-mode 1)                           ; Iterate through CamelCase words


(setq evil-vsplit-window-right t
      evil-split-window-below t)

;; Appearance
(setq doom-font (font-spec :family "Source Code Pro" :size 14)
      doom-big-font (font-spec :family "Source Code Pro" :size 24)
      doom-variable-pitch-font (font-spec :family "Source Code Pro" :size 14)
      doom-serif-font (font-spec :family "IBM Plex Mono" :weight 'light))
(setq doom-theme 'doom-vibrant)
(delq! t custom-theme-load-path)
(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))
(setq display-line-numbers-type 'relative)

(custom-set-faces! '(doom-modeline-evil-insert-state :weight bold :foreground "#339CDB"))

(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

;; Company
(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (setq company-show-numbers t)
(add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.
(setq-default history-length 1000)
(setq-default prescient-history-length 1000)

;; Tide
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))
(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; Flyspell
;;(after! flyspell (require 'flyspell-lazy) (flyspell-lazy-mode 1)) ;; fixes slow typing

;; Projectile
(setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/"))
(defun projectile-ignored-project-function (filepath)
  "Return t if FILEPATH is within any of `projectile-ignored-projects'"
  (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))

;; lsp
(use-package! lsp-mode
  :hook ((c-mode          ; clangd
        c++-mode        ; clangd
        c-or-c++-mode   ; clangd
        js-mode         ; ts-ls (tsserver wrapper)
        js-jsx-mode     ; ts-ls (tsserver wrapper)
        typescript-mode ; ts-ls (tsserver wrapper)
        js2-mode
        typescript-tsx-mode
        javascript-tsx-mode
        python-mode     ; mspyls
        web-mode
        ) . lsp)
  :commands lsp
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-idle-delay 0.5)
  (setq lsp-prefer-capf t)
  (add-to-list 'lsp-language-id-configuration '(js-jsx-mode . "javascriptreact")))
(use-package! company-lsp
  :commands company-lsp)
(use-package! lsp-ui
  :commands
  lsp-ui-mode)

;; company
(use-package company
  :hook (js2-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (setq company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                            company-echo-metadata-frontend))
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "C-j") nil) ; avoid conflict with emmet-mode
    (define-key company-active-map (kbd "C-n") #'company-select-next)
    (define-key company-active-map (kbd "C-p") #'company-select-previous)))

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
         ("\\.jsx?\\'"  . web-mode)
         ("\\.tsx?\\'"  . web-mode)
         ("\\.json\\'"  . web-mode))
  :config
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

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

; (defun setup-tide-mode ()
;  (interactive)
;  (tide-setup)
;  (flycheck-mode +1)
;  (setq flycheck-check-syntax-automatically '(save mode-enabled))
;  (company-mode +1)
;  (tide-mode +1)
;  (tide-restart-server)
;  (set-company-backend! 'js2-mode 'company-tide)
;  )

;; Javascript
(add-hook! 'typescript-tsx-mode-hook
  (prettier-js-mode)
  (setq tab-width 2)
  (setq c-basic-indent 2)
  (setq tab-width 2)
  (setq c-basic-offset 2)
  (setq-default typescript-indent-level 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  )
(add-hook! 'javascript-tsx-mode-hook
  (prettier-js-mode))
(add-hook! 'js2-mode-hook
  (prettier-js-mode))
  ; (setup-tide-mode))
(add-hook! 'typescript-mode-hook
  (prettier-js-mode))
(add-hook! 'web-mode-hook
  (prettier-js-mode)
  (setq tab-width 2)
  (setq c-basic-indent 2)
  (setq tab-width 2)
  (setq c-basic-offset 2)
  (setq-default typescript-indent-level 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  )

;; Python
(add-hook! 'python-mode-hook
  ;; nothing yet
  )

;; Load submodules
(load! "functions.el")
(load! "theming.el")
(load! "bindings.el")
(load! "commands.el")
