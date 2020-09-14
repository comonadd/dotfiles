(when (eq system-type 'gnu/linux)
  (setq x-super-keysym 'meta))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta))

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
  "M-n"      #'mc/mark-next-like-this
  "M-p"      #'mc/mark-previous-like-this
  "<f2>"     #'lsp-rename
  "<f7>"     #'lsp-ui-peek-find-references
  "M-b"      #'lsp-goto-implementation

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

    :desc "Find file content"      :n  "f"   #'counsel-projectile-ag
    :desc "Find project"           :n  "p"   #'projectile-switch-project

    :desc "Delete the window"      :n  "q"   #'delete-window
    :desc "Ivy open buffers"       :n  "b"   #'ivy-switch-buffer

    ;;:desc "Search symbols in file"    :n  "s"   #'lsp-ui-imenu
    :desc "Search symbols in file"    :n  "/"   #'lsp-ivy-workspace-symbol
    :desc "Search symbols in project" :n  "?"   #'lsp-ivy-global-workspace-symbol

    :n "p" nil
    :n "p p" nil
    (:desc "project" :prefix "p"
     :desc "Find file in project"           :n "f"   #'projectile-find-file
     :desc "Switch project"                 :n "s"   #'projectile-switch-project
     :desc "Switch to project buffer"       :n "s"   #'projectile-switch-to-buffer
     :desc "Search in project"              :n "/"   #'+ivy/project-search
    )

    :n "q" nil
    :n "q q" nil
    (:desc "q" :prefix "q"
     :desc "Quit"               :n "q" '(lambda () (interactive) (evil-quit))
     :desc "Restart"            :n "r" '(lambda () (interactive) (restart-emacs))
    )

    :n "f" nil
    :n "f f" nil
    (:desc "f" :prefix "f"
     :desc "Find file"          :n "f" #'find-file
     :desc "Save file"          :n "s" #'save-buffer
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

  ;; Javascript
  (:map typescript-tsx-mode-map
       :n "<f9>"    #'prettier-js)
  (:map javascript-tsx-mode-map
       :n "<f9>"    #'prettier-js)
  (:map js2-mode-map
       :n "<f9>"    #'prettier-js)
  (:map typescript-mode-map
       :n "<f9>"    #'prettier-js)
  (:map web-mode-map
       :n "<f9>"    #'prettier-js)

  ;; Python
  (:map python-mode-map
       :n "<f9>"    #'blacken-buffer)
)

(after! magit
  :map magit-blame-mode-map
  :n "q" #'magit-blame-quit)

