(defun my/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

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
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))
