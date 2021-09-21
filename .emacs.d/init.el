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

;; Disable emacs vc
(setq vc-handled-backends nil)

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
(when (eq system-type 'darwin)
  (defun set-exec-path-from-shell-PATH ()
    "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
  This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
    (interactive)
    (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator))))
  (set-exec-path-from-shell-PATH)
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
(when (eq system-type 'windows-nt)
  ;; (setq w32-allow-system-shell t) ; enables cmd.exe as shell
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
;;  (message exec-path)
  ;; (set-exec-path-from-shell-PATH)
  (toggle-frame-maximized) ; Start maximized
  )

(with-eval-after-load 'counsel
  (when (eq system-type 'windows-nt)
    (defun counsel-locate-cmd-es (input)
      "Return a shell command based on INPUT."
      (counsel-require-program "es.exe")
      (format "es.exe -r %s"
              (counsel--elisp-to-pcre
               (ivy--regex input t))))))

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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#3cafa5")
 '(cua-normal-cursor-color "#8d9fa1")
 '(cua-overwrite-cursor-color "#c49619")
 '(cua-read-only-cursor-color "#93a61a")
 '(custom-enabled-themes '(naysayer))
 '(custom-safe-themes
   '("4eb6fa2ee436e943b168a0cd8eab11afc0752aebb5d974bba2b2ddc8910fca8f" "6bdcff29f32f85a2d99f48377d6bfa362768e86189656f63adbf715ac5c1340b" "b5e75f219d41e6e3516560ac493d808b621a99847d6128ce8e6c74b1495ce875" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "d14f3df28603e9517eb8fb7518b662d653b25b26e83bd8e129acea042b774298" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "aba75724c5d4d0ec0de949694bce5ce6416c132bb031d4e7ac1c4f2dbdd3d580" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "c665e247614a990c845ba7e450c6f8141a9fc1cd3e7e7e0c2ed94530f2de65f8" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" "5eed5311ae09ed84cb2e4bf2f033eb4df27e7846a68e4ea3ab8d28f6b017e44a" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "df01ad8d956b9ea15ca75adbb012f99d2470f33c7b383a8be65697239086672e" "30b14930bec4ada72f48417158155bc38dd35451e0f75b900febd355cda75c3e" "16ce45f31cdea5e74ca4d27519d7ebe998d69ec3bf7df7be63c5ffdb5638b387" "f956b10d80e774db159dadaf43429b334316f69becbd6037ee023833fb35e4bd" "57e3f215bef8784157991c4957965aa31bac935aca011b29d7d8e113a652b693" "801a567c87755fe65d0484cb2bded31a4c5bb24fd1fe0ed11e6c02254017acb2" "dbade2e946597b9cda3e61978b5fcc14fa3afa2d3c4391d477bdaeff8f5638c5" "a5d04a184d259f875e3aedbb6dbbe8cba82885d66cd3cf9482a5969f44f606c0" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "039c01abb72985a21f4423dd480ddb998c57d665687786abd4e16c71128ef6ad" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "5d59bd44c5a875566348fa44ee01c98c1d72369dc531c1c5458b0864841f887c" "45feb1f130c54e0fc116faa71c784562b41009ffc908cf5cef06b6df4bb60a9a" "ea3a76318adf9660a90e1d587233bd05316e05f9fbdaeba7b52399e6dbf9cfd0" "5f824cddac6d892099a91c3f612fcf1b09bb6c322923d779216ab2094375c5ee" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "4780d7ce6e5491e2c1190082f7fe0f812707fc77455616ab6f8b38e796cbffa9" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "6b80b5b0762a814c62ce858e9d72745a05dd5fc66f821a1c5023b4f2a76bc910" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "74ba9ed7161a26bfe04580279b8cad163c00b802f54c574bfa5d924b99daa4b9" "37a4701758378c93159ad6c7aceb19fd6fb523e044efe47f2116bc7398ce20c9" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "cab317d0125d7aab145bc7ee03a1e16804d5abdfa2aa8738198ac30dc5f7b569" "39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "7575474658c34b905bcec30a725653b2138c2f2d3deef0587e3abfae08c5b276" "d4131a682c4436bb5a61103d9a850bf788cbf793f3fd8897de520d20583aeb58" "3273d17945773c9000304645a888ca882c7ed69afb8ca761f6f830dca1788b26" default))
 '(fci-rule-color "#383838")
 '(fringe-mode 10 nil (fringe))
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
 '(hl-sexp-background-color "#efebe9")
 '(hl-todo-keyword-faces
   '(("TODO" . "#FF0000")
     ("FIXME" . "#FF0000")
     ("DEBUG" . "#A020F0")
     ("\\@PERFORMANCE" . "#FF4500")
     ("\\@ROBUSTNESS" . "#1E90FF")))
 '(ivy-count-format "(%d/%d) ")
 '(ivy-use-virtual-buffers t)
 '(jdee-db-active-breakpoint-face-colors (cons "#f1ece4" "#7382a0"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f1ece4" "#81895d"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f1ece4" "#b9a992"))
 '(linum-format " %6d ")
 '(lsp-ui-doc-border "#9eacac")
 '(main-line-color1 "#222232")
 '(main-line-color2 "#333343")
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(objed-cursor-color "#955f5f")
 '(package-selected-packages
   '(auto-complete naysayer haskell-mode esup solarized-theme waher-theme dockerfile-mode gruvbox-theme spacemacs-theme undo-tree sublime-themes ample-theme zenburn-theme lua-mode espresso-theme clues-theme tango-plus-theme apropospriate-theme leuven-theme yaml-mode counsel all-the-icons-ivy-rich ivy-rich flycheck hl-todo string-inflection cmake-mode ag emmet-mode prettier-js php-mode glsl-mode naysayer-theme spaceline spaceline-config eltbus rust-mode treemacs-all-the-icons treemacs-projectile treemacs-evil lsp-treemacs all-the-icons zzz-to-char elisp-format lsp-ivy projectile evil-nerd-commenter bm evil-mc evil-magit magit evil-collection use-package))
 '(pdf-view-midnight-colors (cons "#605a52" "#f7f3ee"))
 '(pos-tip-background-color "#01323d")
 '(pos-tip-foreground-color "#9eacac")
 '(powerline-color1 "#222232")
 '(powerline-color2 "#333343")
 '(rustic-ansi-faces
   ["#f7f3ee" "#955f5f" "#81895d" "#957f5f" "#7382a0" "#9c739c" "#5f8c7d" "#605a52"])
 '(safe-local-variable-values
   '((projectile-project-compilation-cmd . "npm run-script build-dev")
     (projectile-project-compilation-cmd . "cd build && make")
     (projectile-project-compilation-cmd . "npm run-script build-examples")))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#93a61a" "#01323d" 0.2))
 '(telephone-line-mode t)
 '(term-default-bg-color "#002732")
 '(term-default-fg-color "#8d9fa1")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3")
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
 '(default ((t (:background nil))))
 '(markdown-code-face ((t nil))))
