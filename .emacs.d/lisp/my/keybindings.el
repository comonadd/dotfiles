;;; my/keybindings --- Package that defines the keybindings
;;; Commentary:
;;; Code:

(require 'undo-tree)
(require 'multiple-cursors)
(require 'expand-region)
(require 'string-inflection)
(require 'my/util)

(defvar my/keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Emacs-specific keybindings
    (define-key map (kbd "C-q")     'save-buffers-kill-terminal)
    (define-key map (kbd "C-p")     'execute-extended-command)
    (define-key map (kbd "<f1>")    'eval-region)
    (define-key map (kbd "<M-f1>")  'eval-last-sexp)

    ;; Filesystem navigation
    (define-key map (kbd "C-f")     'find-file)
    (define-key map (kbd "C-S-f")   'find-file-other-window)
    (define-key map (kbd "<f11>")   'my/util/open-same-file-other-window)
    (define-key map (kbd "<f2>")    'neotree-toggle)

    ;; In-file serch/replace
    (define-key map (kbd "C-r")     'query-replace)
    (define-key map (kbd "C-e")     'isearch-forward)
    (define-key map (kbd "C-S-e")   'isearch-repeat-forward)

    ;; Window manipulation
    (define-key map (kbd "C-;")     'other-window)
    (define-key map (kbd "C-w")     'kill-this-buffer)
    (define-key map (kbd "C-S-w")   'kill-buffer-and-window)

    ;; Save buffers
    (define-key map (kbd "C-s")    'save-buffer)
    (define-key map (kbd "C-S-s")  'my/util/save-all-buffers)

    ;; Font size manipulation
    (define-key map (kbd "C-=")    'text-scale-increase)
    (define-key map (kbd "C--")    'text-scale-decrease)

    ;; Buffer navigation
    (define-key map (kbd "C-M-j")  nil)
    (define-key map (kbd "C-M-l")  nil)
    (define-key map (kbd "C-M-i")  nil)
    (define-key map (kbd "C-M-k")  nil)
    (define-key map (kbd "M-j")    'backward-char)
    (define-key map (kbd "M-l")    'forward-char)
    (define-key map (kbd "M-i")    'previous-line)
    (define-key map (kbd "M-k")    'next-line)
    (define-key input-decode-map [?\C-i]
      [C-i])
    (define-key map (kbd "<C-i>")  'backward-paragraph)
    (define-key map (kbd "C-S-i")  'my/util/select-backward-paragraph)
    (define-key map (kbd "C-k")    'forward-paragraph)
    (define-key map (kbd "C-S-k")  'my/util/select-forward-paragraph)
    (define-key map (kbd "C-j")    'backward-word)
    (define-key map (kbd "C-l")    'forward-word)
    (define-key map (kbd "C-g")    'goto-line)
    (define-key map (kbd "M-o")    'newline)
    (define-key map (kbd "C-t")    'ace-jump-mode)

    ;; Editing
    (define-key map (kbd "C-y")      'kill-ring-save)
    (define-key map (kbd "C-v")      'yank)
    (define-key map (kbd "C-x")      'kill-region)
    (define-key map (kbd "C-a")      'mark-whole-buffer)
    (define-key map (kbd "C-o")      'my/util/insert-line-above)
    (define-key map (kbd "C-S-o")    'my/util/insert-line-below)
    (define-key map (kbd "M-h")      'delete-backward-char)
    (define-key map (kbd "M-d")      'delete-forward-char)
    (define-key map (kbd "C-h")      'my/util/backward-delete-word)
    (define-key map (kbd "C-d")      'my/util/forward-delete-word)
    (define-key map (kbd "M-n")      'my/util/delete-to-beginning-of-line)
    (define-key map (kbd "M-m")      'my/util/delete-to-end-of-line)
    (define-key map (kbd "<f6>")     'string-inflection-all-cycle)

    ;; Comment/Uncomment
    (define-key map (kbd "C-/")  'my/util/toggle-comment-region-or-line)

    ;; Map escape to cancel (like C-g)...
    (define-key isearch-mode-map [escape] 'isearch-abort)
    (define-key isearch-mode-map "\e" 'isearch-abort)
    (define-key map (kbd "<escape>") 'keyboard-escape-quit)

    ;; Undo Tree package
    (global-undo-tree-mode 1)
    (define-key map (kbd "C-z") 'undo)
    (defalias 'redo 'undo-tree-redo)
    (define-key map (kbd "C-S-z") 'redo)

    ;; Multiple Cursors package
    (define-key map (kbd "C-.") 'mc/mark-next-like-this)
    (define-key map (kbd "C-,") 'mc/mark-previous-like-this)
    (define-key input-decode-map [?\C-m]
      [C-m])
    (define-key map (kbd "<C-m>") 'mc/mark-all-like-this)

    ;; Expand-region package
    (define-key map (kbd "C-'") 'er/expand-region) map)
  "Minor mode keymap.")

(define-minor-mode my/keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")

(my/keys-minor-mode 1)

(defun my/keys-have-priority (_file)
  "Try to ensure that my keybindings retain priority over other minor modes.
Called via the `after-load-functions' special hook."
  (unless (eq (caar minor-mode-map-alist) 'my-keys-minor-mode)
    (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
      (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist mykeys))))

(add-hook 'after-load-functions 'my/keys-have-priority)

;; Fix mouse
(global-unset-key [mouse-2])
(global-unset-key [mouse-1])

(provide 'my/keybindings)
;;; keybindings.el ends here
