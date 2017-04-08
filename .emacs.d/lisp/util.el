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

(defun find-corresponding-file ()
  "Find the file that corresponds to this one."
  (interactive)
  (let ((corresponding-file-name) (base-file-name (buffer-get-base-file-name)))
    (progn
      (cond
       ((string-match "\\.cpp" buffer-file-name) (setq corresponding-file-name (concat base-file-name ".hpp")))
       ((string-match "\\.hpp" buffer-file-name) (setq corresponding-file-name (concat base-file-name ".cpp")))
       ((string-match "\\.c" buffer-file-name) (setq corresponding-file-name (concat base-file-name ".h")))
       ((string-match "\\.h" buffer-file-name) (setq corresponding-file-name (concat base-file-name ".c"))))
      (if corresponding-file-name
          (find-file corresponding-file-name)
        (error "Unable to find a corresponding C file")))))

(defun find-corresponding-file-other-window ()
  "Find the file that corresponds to this one."
  (interactive)
  (find-file-other-window buffer-file-name)
  (find-corresponding-file)
  (other-window -1))

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

(defun save-all-buffers ()
  (interactive)
  (save-some-buffers t))

(provide 'util)
;;; util.el ends here
