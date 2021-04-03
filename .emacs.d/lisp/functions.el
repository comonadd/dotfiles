(defun lispy-format-buffer ()
  (interactive)
  (mark-whole-buffer)
  (lispy-multiline))

(defun save-all ()
  (interactive)
  (save-some-buffers t))

(defun open-config ()
  (interactive)
  (find-file my/config-file))

(defun reload-config ()
  (interactive)
  (load my/config-file))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(setq alt-ext-mapping (make-hash-table :test 'equal))
(puthash "c" '("h") alt-ext-mapping)
(puthash "h" '("c" "cpp") alt-ext-mapping)
(puthash "cpp" '("hpp" "hxx" "h") alt-ext-mapping)
(puthash "hpp" '("cpp") alt-ext-mapping)
(puthash "hxx" '("cpp") alt-ext-mapping)
(puthash "js" '("css" "scss") alt-ext-mapping)
(puthash "jsx" '("css" "scss") alt-ext-mapping)
(puthash "css" '("js" "jsx") alt-ext-mapping)
(puthash "scss" '("js" "jsx") alt-ext-mapping)
(defun my/switch-to-alt-file ()
  "Switch to an alternative version of the current file.
cpp -> hpp, hpp -> cpp, index.js -> index.scss"
  (interactive)
  (setq file-extension (file-name-extension buffer-file-name))
  (setq file-base-name (file-name-sans-extension buffer-file-name))
  (setq file-alt-extension-variants
    (gethash file-extension alt-ext-mapping))
  (while file-alt-extension-variants
    (setq possible-alt-ext (car file-alt-extension-variants))
    (setq possible-alt-file
      (concat file-base-name "." possible-alt-ext))
    (if (file-exists-p possible-alt-file)
      (progn
        (find-file possible-alt-file)
        (setq file-alt-extension-variants '()))
      (setq file-alt-extension-variants
        (cdr file-alt-extension-variants)))))

(defun my/view-current-file-other-window ()
  "Views current file in other window"
  (interactive)
  (let ((filename (buffer-file-name)))
    (find-file-other-window filename)))

(defun my/rename-current-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let
    (
      (name (buffer-name))
      (filename (buffer-file-name)))
    (if (not filename)
      (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun my/delete-current-file ()
  (interactive)
  (if (y-or-n-p (concat "Delete " buffer-file-name "?"))
    (progn
      (delete-file buffer-file-name)
      (kill-buffer))
    (progn
      ;; do nothing
      )))

(defun my/executable-find ()
  "Search for COMMAND in `exec-path' and return the absolute file name.
Return nil if COMMAND is not found anywhere in `exec-path'."
  (interactive)
  (setq command (read-string "Name: "))
  (message (concat "Searching for \"" command "\" in exec-path"))
  ;; Use 1 rather than file-executable-p to better match the behavior of
  ;; call-process.
  (message
    (concat
      "Result: "
      (locate-file command exec-path exec-suffixes 1))))


(defun my/read-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun my/read-dir-locals ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (my/read-dir-locals-for-current-buffer)))))
