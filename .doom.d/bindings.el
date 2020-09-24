(when (eq system-type 'gnu/linux)
  (setq x-super-keysym 'meta))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'alt))

(map!
 ;; Window Movements
 "C-h"      #'evil-window-left
 "C-j"      #'evil-window-down
 "C-k"      #'evil-window-up
 "C-l"      #'evil-window-right
 "<C-tab>"  #'+popup/other
 "<f8>"     #'lsp-execute-code-action
 :n "M-."   nil
 :n "M-."   #'lsp-execute-code-action
 "<f2>"     #'lsp-rename
 "<f7>"     #'lsp-find-references
 "M-b"      #'xref-find-definitions
 "<f11>"    #'calc
 "M-v"      nil
 "M-H-l"    nil
 "A-d"      #'evil-delete-whole-line
 "M-`"      nil
 "M-/"      #'evilnc-comment-or-uncomment-lines

 ;; Leader Configs
 (:leader
  "SPC"      :n nil
  :desc "Execute"                :n  "SPC"     #'execute-extended-command
  :desc "Cycle between buffers"  :nv "<tab>"   #'mode-line-other-buffer
  :desc "Neotree toggle"         :n  "n"       #'rschmukler/neotree-project-root-dir-or-current-dir

  :desc "Horizonal Split"        :n  "s"   #'split-window-below
  :desc "Vertical Split"         :n  "v"   #'split-window-right

  :desc "Next Error"             :n  "]"   #'flycheck-next-error
  :desc "Previous Error"         :n  "["   #'flycheck-previous-error
  :desc "Show flycheck errors"   :n  "!"   #'flycheck-list-errors

  :desc "Find project"           :n  "p"   #'projectile-switch-project

  :desc "Delete the window"      :n  "q"   #'delete-window
  :desc "Ivy open buffers"       :n  "b"   #'ivy-switch-buffer

  ;;:desc "Search symbols in file"    :n  "s"   #'lsp-ui-imenu
  :desc "Search symbols in file" :n  "/"   #'swiper-isearch
  :desc "Search symbols in file" :n  "?"   #'lsp-ivy-workspace-symbol

  :n "p" nil
  :n "p p" nil
  (:desc "project" :prefix "p"
   :desc "Find file in project"           :n "f"   #'projectile-find-file
   :desc "Switch project"                 :n "s"   #'projectile-switch-project
   :desc "Search in project"              :n "t"   #'+ivy/project-search
   :desc "Search symbols in project"      :n "i"   #'lsp-ivy-global-workspace-symbol
   :desc "Replace in project"             :n "w"   #'projectile-replace
   )

  :n "q" nil
  :n "q q" nil
  (:desc "q" :prefix "q"
   :desc "Quit"               :n "q" '(lambda () (interactive) (kill-emacs))
   :desc "Restart"            :n "r" '(lambda () (interactive) (restart-emacs))
   )

  ;; Filesystem related keybindings
  :n "f" nil
  :n "f f" nil
  (:desc "f" :prefix "f"
   :desc "Find file"          :n "f" #'find-file
   :desc "Save file"          :n "s" #'save-buffer
   :desc "Copy file name"     :n "y" #'+default/yank-buffer-filename
   :desc "Open treemacs"      :n "t" #'treemacs
   :desc "Open a recent file" :n "r" #'recentf-open-files
   :desc "Rename buffer"      :n "R" #'rename-current-buffer-file
   )

  (:desc "help" :prefix "h"
   :n "h" help-map
   :desc "Apropos"               :n "a" #'apropos
   :desc "Reload theme"          :n "R" #'doom/reload-theme
   :desc "Find library"          :n "l" #'find-library
   :desc "Toggle Emacs log"      :n "m" #'doom/popup-toggle-messages
   :desc "Command log"           :n "L" #'global-command-log-mode
   :desc "Describe function"     :n "f" #'describe-function
   :desc "Describe key"          :n "k" #'describe-key
   :desc "Describe char"         :n "c" #'describe-char
   :desc "Describe mode"         :n "M" #'describe-mode
   :desc "Show messages"         :n "m" #'view-echo-area-messages
   :desc "Describe variable"     :n "v" #'describe-variable
   :desc "Describe face"         :n "F" #'describe-face
   :desc "Describe DOOM setting" :n "s" #'doom/describe-setting
   :desc "Describe DOOM module"  :n "d" #'doom/describe-module
   :desc "Find definition"       :n "." #'+jump/definition
   :desc "Find references"       :n "/" #'+jump/references
   :desc "Find documentation"    :n "h" #'+jump/documentation
   :desc "What face"             :n "'" #'doom/what-face
   :desc "What minor modes"      :n ";" #'doom/what-minor-mode
   :desc "Info"                  :n "i" #'info
   :desc "Toggle profiler"       :n "p" #'doom/toggle-profiler)

  (:desc "git" :prefix "g"
   :desc "Git status"        :n  "s" #'magit-status
   :desc "Git blame"         :n  "b" #'magit-blame
   :desc "Git timemachine branch" :n  "B" #'git-timemachine-switch-branch
   :desc "Git time machine"  :n  "t" #'git-timemachine-toggle
   :desc "Git revert hunk"   :n  "r" #'git-gutter:revert-hunk
   :desc "List gists"        :n  "g" #'+gist:list
   :desc "Next hunk"         :nv "]" #'git-gutter:next-hunk
   :desc "Previous hunk"     :nv "[" #'git-gutter:previous-hunk)
  )
 )


;; Javascript
(map! :map typescript-tsx-mode-map :n "A-M-l" #'prettier-js)
(map! :map javascript-tsx-mode-map :n "A-M-l" #'prettier-js)
(map! :map js2-mode-map :n "A-M-l" #'prettier-js)
(map! :map typescript-mode-map :n "A-M-l" #'prettier-js)
(map! :map web-mode-map :n "A-M-l" #'prettier-js)

;; Python
(map! :map python-mode-map :n "A-M-l" #'blacken-buffer)

;; Rust
(map! :map rust-mode-map :n "A-M-l" #'rust-format-buffer)
(map! :map rustic-mode-map :n "A-M-l" #'rustic-format-buffer)

(after! magit
  :map magit-blame-mode-map
  :n "q" #'magit-blame-quit)
