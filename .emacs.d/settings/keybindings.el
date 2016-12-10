;; Navigation
(global-set-key (kbd "C-f") 'find-file)
(global-set-key (kbd "C-S-f") 'find-file-other-window)
(global-set-key (kbd "C-r") 'query-replace)
(global-set-key (kbd "C-e") 'isearch-forward)
(global-set-key (kbd "C-S-e") 'isearch-repeat-forward)
(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "C-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-w") 'kill-this-buffer)
(global-set-key (kbd "C-p") 'execute-extended-command)
(global-set-key (kbd "C-4") 'move-end-of-line)
(global-set-key (kbd "C-\\") 'move-beginning-of-line)
(global-set-key (kbd "C-g") 'goto-line)
(global-set-key (kbd "<f6>") 'imenu)

;; Editing
(global-set-key (kbd "C-c") 'kill-ring-save)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-d") 'kill-line)
(global-set-key (kbd "C-x") 'kill-region)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-S-s") 'save-some-buffers)
(global-set-key (kbd "C-/") 'toggle-comment-region-or-line)

;; Folding
(global-set-key (kbd "C-<f9>") 'hs-show-block)
(global-set-key (kbd "S-<f9>") 'hs-hide-all)
(global-set-key (kbd "S-C-<f9>") 'hs-show-all)

;; Not distributed
(global-set-key (kbd "<f12>") 'eval-last-sexp)
(global-set-key (kbd "<backtab>") 'indent-for-tab-command)

;; BS plugin
(global-set-key (kbd "<f2>") 'bs-show) ;; запуск buffer selection кнопкой F2

(provide 'keybindings)
