;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     typescript
     ivy
     better-defaults
     emacs-lisp
     git
     org
     (markdown :variables markdown-live-preview-engine 'vmd)
     version-control
     (c-c++ :variables c-c++-enable-clang-support t)
     (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-complete-with-key-sequence-delay 0.1
                      auto-completion-private-snippets-directory nil
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t)
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     syntax-checking
     semantic
     themes-megapack)
   dotspacemacs-additional-packages '(pretty-mode
                                      glsl-mode)
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(smartparens)
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(wilson zenburn obsidian graham espresso)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Inconsolata"
                               :size 14
                               :antialias t
                               :weight bold
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers 'relative
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'trailing))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (setq custom-file "~/.emacs-custom.el")
  (load custom-file))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;; Set safe local variables
  (put 'helm-make-build-dir 'safe-local-variable 'stringp)

  ;; [Semantic bug fix](https://github.com/company-mode/company-mode/issues/525)
  (eval-after-load 'semantic
    (add-hook 'semantic-mode-hook
              (lambda ()
                (dolist (x (default-value 'completion-at-point-functions))
                  (when (string-prefix-p "semantic-" (symbol-name x))
                    (remove-hook 'completion-at-point-functions x))))))

  ;; Configure projectile "other file" feature
  (with-eval-after-load 'projectile
    (push '("C" "h") projectile-other-file-alist)
    (push '("c" "h") projectile-other-file-alist)
    (push '("cpp" "hpp") projectile-other-file-alist)
    (push '("cxx" "hxx") projectile-other-file-alist))

  ;; Configure C Default Style
  (c-add-style "bb"
               '((indent-tabs-mode . nil)
                 (c-basic-offset . 2)
                 (c-offsets-alist
                  (substatement-open . 0)
                  (inline-open . 0)
                  (innamespace . 0)
                  (inextern-lang . 0)
                  (string . 0)
                  (c . 1)
                  (arglist-close . 0))))
  (push '(other . "bb") c-default-style)

  ;; Bind keys
  (define-key evil-normal-state-map (kbd "<f8>") 'clang-format-buffer)
  (define-key evil-normal-state-map (kbd "C-c =") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
  (define-key evil-normal-state-map (kbd "<f9>") 'fill-region)

  ;; Make faces in company tooltips look better
  (custom-set-faces
   '(company-tooltip-common
     ((t (:inherit company-tooltip :weight bold :underline nil))))
   '(company-tooltip-common-selection
     ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))

  ;; Configure auto-mode-alist
  (add-to-list 'auto-mode-alist '("\\.shader\\'" . glsl-mode))

  ;; Configure fonts
  (require 'pretty-mode)
  (global-pretty-mode t)
  (pretty-activate-groups
   '(:sub-and-superscripts :greek :arithmetic-nary))
  (global-prettify-symbols-mode 1)
  (add-hook
   'python-mode-hook
   (lambda ()
     (mapc (lambda (pair) (push pair prettify-symbols-alist))
           '(;; Syntax
             ("def" .      #x2131)
             ("not" .      #x2757)
             ("in" .       #x2208)
             ("not in" .   #x2209)
             ("return" .   #x27fc)
             ("yield" .    #x27fb)
             ("for" .      #x2200)
             ;; Base Types
             ("int" .      #x2124)
             ("float" .    #x211d)
             ("str" .      #x1d54a)
             ("True" .     #x1d54b)
             ("False" .    #x1d53d)
             ;; Mypy
             ("Dict" .     #x1d507)
             ("List" .     #x2112)
             ("Tuple" .    #x2a02)
             ("Set" .      #x2126)
             ("Iterable" . #x1d50a)
             ("Any" .      #x2754)
             ("Union" .    #x22c3)))))
  (add-hook
   'c++-mode-hook
   (lambda ()
     (mapc (lambda (pair) (push pair prettify-symbols-alist))
           '(;; Syntax
             ("not" .      #x2757)
             ("return" .   #x27fc)
             ("yield" .    #x27fb)
             ("for" .      #x2200)
             ("union" .    #x22c3)
             ;; Base Types
             ("string" .   #x1d54a)
             ("true" .     #x1d54b)
             ("false" .    #x1d53d)
             ;; Mypy
             ("map" .     #x1d507)
             ("list" .     #x2112)
             ("tuple" .    #x2a02)
             ("set" .      #x2126)
             ("iterator" . #x1d50a)
             ))))

  ;; Configure the margin line
  (setq-default fill-column 100)
  (add-hook 'markdown-mode-hook 'turn-on-fci-mode))
