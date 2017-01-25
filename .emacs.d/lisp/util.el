;;; Util --- Main Emacs utility file
;;; Commentary:
;;;   This file defines all utility-like functions
;;; Code:

(require 'compile)

(defun forward-kill-word ()
  "If `point' is followed by whitespace kill that.
Otherwise call `kill-word'"
  (interactive)
  (if (looking-at "[ \t\n]")
      (let ((pos (point)))
	(re-search-forward "[^ \t\n]" nil t)
	(backward-char)
	(kill-region pos (point)))
    (kill-word 1)))

(defun create-scratch-buffer nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun insert-current-date ()
  "Insert current date."
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(defun toggle-comment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun ff-find-other-file-other-window ()
  "Switch between file extensions in other window."
  (interactive "r")
  (let ((oldbuf (buffer-name)))
    (sql-send-region)
    (switch-to-buffer oldbuf)))

(provide 'util)
;;; util.el ends here
