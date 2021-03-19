;;
;;; Global variables
(setq-default my/user-root (file-name-as-directory (getenv "HOME")))
(setq-default my/emacs-root
  (file-name-as-directory (concat my/user-root ".emacs.d")))
(setq-default my/config-root
  (file-name-as-directory (concat my/user-root ".emacs.d/lisp")))
(setq-default my/third-party-root
  (file-name-as-directory (concat my/config-root "third-party")))
(setq-default my/config-file (concat my/emacs-root "init.el"))
(setq-default my/projects-root (concat my/user-root "Projects"))

;;
;;; Initialize the package manager
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/")
  t)
(package-initialize)

;;
;;; Emacs Lisp path configuration
(add-to-list 'load-path (expand-file-name my/config-root))

;;
;;; OS path configuration

;; MacOS
(if (eq system-type 'darwin)
  (setq exec-path
    (list
      (concat (getenv "HOME") "/.asdf/shims")
      "/usr/local/bin/"
      "/usr/bin/"
      "/bin/"
      "/usr/sbin/"
      "/sbin/"
      (concat (getenv "HOME") "/.cargo/bin")
      (concat (getenv "HOME") "/.local/bin")
      (concat (getenv "HOME") "/Scripts")))
  (setq lsp-pyls-plugins-flake8-filename
    (concat (getenv "HOME") "/.asdf.shims"))
  (setq racer-rust-src-path
    (concat
      (string-trim (shell-command-to-string "rustc --print sysroot"))
      "/lib/rustlib/src/rust/src"))
  (setq flycheck-python-pylint-executable
    (expand-file-name
      (concat
        (file-name-as-directory (getenv "HOME"))
        ".asdf/shims/pylint"))))

;; Windows
(if (eq system-type 'windows-nt)
  (defun set-exec-path-from-shell-PATH ()
    "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
    (interactive)
    (let
      (
        (path-from-shell
          (replace-regexp-in-string
            "[ \t\n]*$" ""
            (shell-command-to-string
              "$SHELL --login -i -c 'echo $PATH'"))))
      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator))))
  (setq default-directory "C:\\Users\\Dmitry")
  (set-exec-path-from-shell-PATH))

;;
;;; Require other configuration parts
(load "basics.el")
(load "packages.el")
(load "functions.el")
(load "modes.el")
(load "keybindings.el")
(load "appearance.el")

;;
;;; Auto-generated stuff

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#3cafa5")
 '(cua-normal-cursor-color "#8d9fa1")
 '(cua-overwrite-cursor-color "#c49619")
 '(cua-read-only-cursor-color "#93a61a")
 '(custom-enabled-themes '(naysayer))
 '(custom-safe-themes
   '("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "5d59bd44c5a875566348fa44ee01c98c1d72369dc531c1c5458b0864841f887c" "45feb1f130c54e0fc116faa71c784562b41009ffc908cf5cef06b6df4bb60a9a" "ea3a76318adf9660a90e1d587233bd05316e05f9fbdaeba7b52399e6dbf9cfd0" "5f824cddac6d892099a91c3f612fcf1b09bb6c322923d779216ab2094375c5ee" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "4780d7ce6e5491e2c1190082f7fe0f812707fc77455616ab6f8b38e796cbffa9" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "6b80b5b0762a814c62ce858e9d72745a05dd5fc66f821a1c5023b4f2a76bc910" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "74ba9ed7161a26bfe04580279b8cad163c00b802f54c574bfa5d924b99daa4b9" "37a4701758378c93159ad6c7aceb19fd6fb523e044efe47f2116bc7398ce20c9" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "cab317d0125d7aab145bc7ee03a1e16804d5abdfa2aa8738198ac30dc5f7b569" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "7575474658c34b905bcec30a725653b2138c2f2d3deef0587e3abfae08c5b276" "d4131a682c4436bb5a61103d9a850bf788cbf793f3fd8897de520d20583aeb58" "3273d17945773c9000304645a888ca882c7ed69afb8ca761f6f830dca1788b26" default))
 '(highlight-changes-colors '("#e2468f" "#7a7ed2"))
 '(highlight-symbol-colors
   '("#3c6e408d329c" "#0c4a45f54ce3" "#486d33913531" "#1fab3bea568c" "#2ec943ac3324" "#449935a6314d" "#0b03411b5985"))
 '(highlight-symbol-foreground-color "#9eacac")
 '(highlight-tail-colors
   '(("#01323d" . 0)
     ("#687f00" . 20)
     ("#008981" . 30)
     ("#0069b0" . 50)
     ("#936d00" . 60)
     ("#a72e01" . 70)
     ("#a81761" . 85)
     ("#01323d" . 100)))
 '(hl-bg-colors
   '("#936d00" "#a72e01" "#ae1212" "#a81761" "#3548a2" "#0069b0" "#008981" "#687f00"))
 '(hl-fg-colors
   '("#002732" "#002732" "#002732" "#002732" "#002732" "#002732" "#002732" "#002732"))
 '(hl-paren-colors '("#3cafa5" "#c49619" "#3c98e0" "#7a7ed2" "#93a61a"))
 '(hl-todo-keyword-faces
   '(("TODO" . "#FF0000")
     ("FIXME" . "#FF0000")
     ("DEBUG" . "#A020F0")
     ("\\@PERFORMANCE" . "#FF4500")
     ("\\@ROBUSTNESS" . "#1E90FF")))
 '(initial-frame-alist '((fullscreen . maximized)))
 '(ivy-count-format "(%d/%d) ")
 '(ivy-use-virtual-buffers t)
 '(jdee-db-active-breakpoint-face-colors (cons "#f1ece4" "#7382a0"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f1ece4" "#81895d"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f1ece4" "#b9a992"))
 '(lsp-ui-doc-border "#9eacac")
 '(objed-cursor-color "#955f5f")
 '(package-selected-packages
   '(counsel all-the-icons-ivy-rich ivy-rich flycheck hl-todo string-inflection cmake-mode ag emmet-mode prettier-js php-mode glsl-mode naysayer-theme spaceline spaceline-config eltbus rust-mode treemacs-all-the-icons treemacs-projectile treemacs-evil lsp-treemacs all-the-icons zzz-to-char company-box elisp-format lsp-ivy company projectile evil-nerd-commenter bm evil-mc evil-magit magit evil-collection use-package))
 '(pdf-view-midnight-colors (cons "#605a52" "#f7f3ee"))
 '(pos-tip-background-color "#01323d")
 '(pos-tip-foreground-color "#9eacac")
 '(rustic-ansi-faces
   ["#f7f3ee" "#955f5f" "#81895d" "#957f5f" "#7382a0" "#9c739c" "#5f8c7d" "#605a52"])
 '(smartrep-mode-line-active-bg (solarized-color-blend "#93a61a" "#01323d" 0.2))
 '(telephone-line-mode t)
 '(term-default-bg-color "#002732")
 '(term-default-fg-color "#8d9fa1")
 '(vc-annotate-background-mode nil)
 '(weechat-color-list
   '(unspecified "#002732" "#01323d" "#ae1212" "#ec423a" "#687f00" "#93a61a" "#936d00" "#c49619" "#0069b0" "#3c98e0" "#a81761" "#e2468f" "#008981" "#3cafa5" "#8d9fa1" "#60767e"))
 '(xterm-color-names
   ["#01323d" "#ec423a" "#93a61a" "#c49619" "#3c98e0" "#e2468f" "#3cafa5" "#faf3e0"])
 '(xterm-color-names-bright
   ["#002732" "#db5823" "#62787f" "#60767e" "#8d9fa1" "#7a7ed2" "#9eacac" "#ffffee"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t nil))))
