;;; Util --- Main Emacs utility file
;;; Commentary:
;;;   This file defines all utility-like functions
;;; Code:

(require 'compile)

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

(defun get-project-root-dir (path)
  "Recursively search for project root directory (it should use Makefile)."
  (interactive)
  (if (file-exists-p (concat path "/Makefile")) path
    (get-project-root-dir (concat path "/.."))))

(defun compile-project-build ()
  "Look for Makefile and execute make -k -C build on it."
  (interactive)
  (compile (concat "make -k -j4 -C " (concat (get-project-root-dir default-directory) " build"))))

(defun compile-project-tests ()
  "Look for Makefile and execute make -k -C tests on it."
  (interactive)
  (compile (concat "make -k -j4 -C " (concat (get-project-root-dir default-directory) " tests"))))

(defun compile-project-debug ()
  "Look for Makefile and execute make -k -C debug on it."
  (interactive)
  (compile (concat "make -k -j4 -C " (concat (get-project-root-dir default-directory) " debug"))))

(defun query-replace-in-dir ()
  "Query replace in all files in the project."
  (interactive)
  (call-interactively 'ido-dired)
  (call-interactively 'dired-toggle-marks)
  (call-interactively 'dired-do-query-replace-regexp))

(defun query-replace-in-project ()
  "Query replace in all files in the project."
  (query-replace-in-dir (get-project-root-dir default-directory)))

(provide 'util)
;;; util.el ends here
