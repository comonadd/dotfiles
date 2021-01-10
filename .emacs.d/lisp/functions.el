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
  (setq file-alt-extension-variants (gethash file-extension alt-ext-mapping))
  (while file-alt-extension-variants
    (setq possible-alt-ext (car file-alt-extension-variants))
    (setq possible-alt-file (concat file-base-name "." possible-alt-ext))
    (if (file-exists-p possible-alt-file)
        (progn (find-file possible-alt-file)
               (setq file-alt-extension-variants '()))
      (setq file-alt-extension-variants (cdr file-alt-extension-variants)))))

(defun my/rename-current-file ()
  (interactive)
  (setq new-file-name (read-string "New file name: "))
  (rename-file buffer-file-name new-file-name))

(defun my/delete-current-file ()
  (interactive)
  (if (y-or-n-p (concat "Delete " buffer-file-name "?"))
      (progn (delete-file buffer-file-name)
             (kill-buffer))
    (progn
      ;; do nothing
      )))
