;;; Util --- Main Emacs utility file
;;; Commentary:
;;;   This file defines all utility-like functions
;;; Code:

(require 'compile)

;; Editing

(defun forward-kill-word ()
  "If `point' is followed by whitespace kill that, otherwise call `kill-word'."
  (interactive)
  (if (looking-at "[ \t\n]")
    (let ((pos (point)))
      (re-search-forward "[^ \t\n]" nil t)
      (backward-char)
      (kill-region pos (point)))
    (kill-word 1)))

(defun toggle-comment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
      (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;; Navigation

(defun ff-find-other-file-other-window ()
  "Switch between file extensions in other window."
  (interactive "r")
  (let ((oldbuf (buffer-name)))
    (sql-send-region)
    (switch-to-buffer oldbuf)))

;; Other

(defun buffer-get-base-file-name ()
  "Get the base file name of a current buffer."
  (file-name-sans-extension
    (file-name-nondirectory buffer-file-name)))

(defun buffer-file-name-nondir ()
  "Get the name of a buffer without directory path."
  (file-name-nondirectory buffer-file-name))

(defun create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun get-current-date ()
  "Insert current date."
  (interactive)
  (shell-command-to-string "echo -n $(date +%Y-%m-%d)"))

(defun open-same-file-other-window ()
  "Open the same file in another window."
  (interactive)
  (switch-to-buffer-other-window (buffer-name)))

(provide 'util)
;;; util.el ends here
