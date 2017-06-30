;;; my/util --- Main Emacs utility file
;;; Commentary:
;;;   This file defines all utility-like functions
;;; Code:

(require 'compile)

(defun my/util/insert-line-above (times)
  "Insert line above the current one TIMES times."
  (interactive "p")
  (save-excursion (move-beginning-of-line 1)
                  (newline times)))

(defun my/util/insert-line-below (times)
  "Insert line below the current one TIMES times."
  (interactive "p")
  (save-excursion (move-end-of-line 1)
                  (newline times)))

(defun my/util/backward-delete-word (n)
  "Delete N backward words."
  (interactive "p")
  (delete-region (point)
                 (save-excursion (backward-word 1)
                                 (point))))

(defun my/util/forward-delete-word (n)
  "Delete N forward words."
  (interactive "p")
  (delete-region (point)
                 (save-excursion (forward-word 1)
                                 (point))))

(defun my/util/toggle-comment-region-or-line ()
  "Comment or uncomment the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun my/util/delete-to-beginning-of-line (n)
  "Delete the text to the beginning of the current line N times."
  (interactive "p")
  (kill-line (- 1 n)))

(defun my/util/delete-to-end-of-line (n)
  "Delete the text to the end of the current line N times."
  (interactive "p")
  (delete-region (point)
                 (save-excursion (move-end-of-line 1)
                                 (point))))

(defun my/util/ff-find-other-file-other-window ()
  "Switch between file extensions in other window."
  (interactive "r")
  (let ((oldbuf (buffer-name)))
    (switch-to-buffer oldbuf)))

(defun my/util/buffer-get-base-file-name ()
  "Get the base file name of a current buffer."
  (file-name-sans-extension (file-name-nondirectory buffer-file-name)))

(defun my/util/buffer-file-name-nondir ()
  "Get the name of a buffer without directory path."
  (file-name-nondirectory buffer-file-name))

(defun my/util/create-scratch-buffer ()
  "Create a new scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun my/util/get-current-date ()
  "Get the current date."
  (interactive)
  (shell-command-to-string "echo -n $(date +\"%d %b %Y\")"))

(defun my/util/open-same-file-other-window ()
  "Open the same file in another window."
  (interactive)
  (switch-to-buffer-other-window (buffer-name)))

(defun my/util/save-all-buffers ()
  "Save all opened buffers."
  (interactive)
  (save-some-buffers t))

(defun my/util/select-forward-paragraph (n)
  "Select N forward paragraphs."
  (interactive "p")
  (set-mark-command nil)
  (forward-paragraph n))

(defun my/util/select-backward-paragraph (n)
  "Select N backward paragraphs."
  (interactive "p")
  (set-mark-command nil)
  (backward-paragraph n))

(provide 'my/util)
;;; util.el ends here
