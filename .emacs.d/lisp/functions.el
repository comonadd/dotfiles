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

(defun my/next-theme ()
  ;; Load next theme in the custom-available-themes list
  (interactive)

  (defun my/load-theme (all-themes themes curr-theme theme-to-load)
    ;; Load a theme with error handling
    (message (concatenate 'string "Loading :: " (format "%s" theme-to-load)))
    ;; Disable previous theme settings
    (disable-theme curr-theme)
    ;; If we fail to load the next theme, try loading the one after that
    (let ((loaded (condition-case nil
              (load-theme theme-to-load t)
              (error 0))))
      (if (eq loaded 0)
        (progn
          (message (format "Failed to load %s, skipping..." theme-to-load))
          (message (format "Length of themes left: %s" (length themes)))
          (consider-theme all-themes (cdr themes) theme-to-load)
          )
        (message (concatenate 'string "Loaded :: " (format "%s" theme-to-load))))))

  (defun consider-theme (all-themes themes theme)
    (cond
     ((null themes) (message "Error: Couldn't find current theme in the list"))
     ((and (eq (car themes) theme) (eq (length themes) 1))
      (progn
        (message (concatenate 'string "Wrapping around..."))
        ;; Load the first theme
        (my/load-theme all-themes all-themes theme (car all-themes))))
     ((eq (car themes) theme)
      (my/load-theme all-themes themes theme (cadr themes)))
     (t (consider-theme all-themes (cdr themes) theme))))
  (let ((themes (custom-available-themes))
        (curr-theme (car custom-enabled-themes)))
    (progn
      (if (null curr-theme)
          (progn
            (message "Failed to detect current theme. Switching to the first one.")
            (load-theme (car themes)))
          (consider-theme themes themes curr-theme)))))

(defun my/reset-themes ()
  (interactive)
  ;; Disable all enabled themes
  (defun rec (themes-to-disable theme)
    (disable-theme theme)
    (if (null themes-to-disable)
      nil
      (rec (cdr themes-to-disable) (car themes-to-disable))))
  (rec custom-enabled-themes (car custom-enabled-themes))
  ;; Enable the first one in the list
  (load-theme (car (custom-available-themes))))
