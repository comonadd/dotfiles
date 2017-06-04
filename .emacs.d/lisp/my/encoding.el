;;; my/encoding --- Package that setups the enconding
;;; Commentary:
;;; Code:

(set-language-environment 'UTF-8)
(setq buffer-file-coding-system 'utf-8)
(setq-default coding-system-for-read 'utf-8)
(setq file-name-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(provide 'my/encoding)
;;; encoding.el ends here
