;;; my/util --- Main Emacs utility file
;;; Commentary:
;;;   This file defines all utility-like functions
;;; Code:

(require 'compile)

;; Editing

(defun my/insert-line-above (times)
  "Insert line above the current one TIMES times."
  (interactive "p")
  (save-excursion
    (move-beginning-of-line 1)
    (newline times)))

(defun my/insert-line-below (times)
  "Insert line below the current one TIMES times."
  (interactive "p")
  (save-excursion
    (move-end-of-line 1)
    (newline times)))

(defun my/backward-delete-word (n)
  "Delete N backward words."
  (interactive "p")
  (delete-region (point)
                 (save-excursion
                   (backward-word 1)
                   (point))))

(defun my/forward-delete-word (n)
  "Delete N forward words."
  (interactive "p")
  (delete-region (point)
                 (save-excursion
                   (forward-word 1)
                   (point))))

(defun my/toggle-comment-region-or-line ()
  "Comment or uncomment the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;; Navigation

(defun my/ff-find-other-file-other-window ()
  "Switch between file extensions in other window."
  (interactive "r")
  (let ((oldbuf (buffer-name)))
    (sql-send-region)
    (switch-to-buffer oldbuf)))

;; Other

(defun my/buffer-get-base-file-name ()
  "Get the base file name of a current buffer."
  (file-name-sans-extension (file-name-nondirectory buffer-file-name)))

(defun my/buffer-file-name-nondir ()
  "Get the name of a buffer without directory path."
  (file-name-nondirectory buffer-file-name))

(defun my/create-scratch-buffer ()
  "Create a new scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun my/get-current-date ()
  "Insert current date."
  (interactive)
  (shell-command-to-string "echo -n $(date +%Y-%m-%d)"))

(defun my/open-same-file-other-window ()
  "Open the same file in another window."
  (interactive)
  (switch-to-buffer-other-window (buffer-name)))

(defun my/save-all-buffers ()
  "Save all opened buffers."
  (interactive)
  (save-some-buffers t))

(defun my/select-forward-paragraph (n)
  "Select N forward paragraphs."
  (interactive "p")
  (set-mark-command nil)
  (forward-paragraph n))

(defun my/select-backward-paragraph (n)
  "Select N backward paragraphs."
  (interactive "p")
  (set-mark-command nil)
  (backward-paragraph n))

(provide 'my/util)
;;; util.el ends here
