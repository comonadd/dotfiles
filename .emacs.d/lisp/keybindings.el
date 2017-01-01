;;; Keybindings --- Keybindings
;;; Commentary:
;; This file defines all keybindings that don't depend on packages
;;; Code:

;; Unsets
(global-unset-key [mouse-2])
(global-unset-key [mouse-1])

;; Emacs-specific keybindings
(global-set-key (kbd "<f1>") 'eval-buffer)

;; Navigation
(global-set-key (kbd "C-f") 'find-file)
(global-set-key (kbd "C-S-f") 'find-file-other-window)
(global-set-key (kbd "C-r") 'query-replace)
(global-set-key (kbd "C-e") 'isearch-forward)
(global-set-key (kbd "C-S-e") 'isearch-repeat-forward)
(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "C-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-w") 'kill-this-buffer)
(global-set-key (kbd "C-S-w") 'kill-buffer-and-window)
(global-set-key (kbd "C-p") 'execute-extended-command)
(global-set-key (kbd "C-4") 'move-end-of-line)
(global-set-key (kbd "C-\\") 'move-beginning-of-line)
(global-set-key (kbd "C-g") 'goto-line)
(global-set-key (kbd "C-t") 'pop-global-mark)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-S-b") 'compile-project-build)
(global-set-key (kbd "C-S-t") 'compile-project-tests)
(global-set-key (kbd "C-S-d") 'compile-project-debug)

;; Editing
(global-set-key (kbd "C-y") 'kill-ring-save)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-S-x") 'kill-region)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-S-s") 'save-some-buffers)
(global-set-key (kbd "C-/") 'toggle-comment-region-or-line)
(global-set-key (kbd "<backtab>") 'indent-for-tab-command)
(global-set-key (kbd "C-i") 'quoted-insert)

;; Refactoring
(global-set-key (kbd "C-S-r") 'query-replace-in-project)

;; Folding
(global-set-key (kbd "C-[") 'hs-show-block)
(global-set-key (kbd "C-{") 'hs-show-all)
(global-set-key (kbd "C-]") 'hs-hide-block)
(global-set-key (kbd "C-}") 'hs-hide-all)

; Map escape to cancel (like C-g)...
(define-key isearch-mode-map [escape] 'isearch-abort)
(define-key isearch-mode-map "\e" 'isearch-abort)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(provide 'keybindings)
;;; keybindings.el ends here
