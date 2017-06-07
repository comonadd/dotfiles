;;; my/jsx-mode/main --- The main file of my JSX mode
;;; Commentary:
;;; Code:

(require 'flycheck)
(require 'jsx-mode)
(require 'web-mode)

(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."
                         :command ("jsxhint" source)
                         :error-patterns ((error
                                           line-start
                                           (1+ nonl)
                                           ": line "
                                           line
                                           ", col "
                                           column
                                           ", "
                                           (message)
                                           line-end))
                         :modes (jsx-mode web-mode))

(defun my/jsx-mode/set-style ()
  "Set the style for the JavaScript mode."
  (setq jsx-indent-level 2))

(defun my/jsx-mode/bind-keys ()
  "Bind keys for the JSX mode.")

(defun my/jsx-mode/add-hooks ()
  "Add hooks for the JSX mode.")

(defun my/jsx-mode/hook ()
  "The JavaScript mode hook."
  (my/jsx-mode/set-style)
  (my/jsx-mode/bind-keys)
  (my/jsx-mode/add-hooks)
  (flycheck-select-checker 'jsxhint-checker)
  (flycheck-mode))

(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil)) ad-do-it) ad-do-it))

;; Add the hook
(add-hook 'jsx-mode-hook 'my/jsx-mode/hook)

(provide 'my/jsx-mode/main)
;;; main.el ends here
