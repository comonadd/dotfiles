;;; my/ebnf-mode --- Highlight mode for Extended Backus-Naur Form
;;; Commentary:
;;; Code:

;;;###autoload
(define-generic-mode 'ebnf-mode '(("(*" . "*)"))
                     '("=")
                     '(("^[^ \t\n][^=]+" . font-lock-variable-name-face)
                       ("['\"].*?['\"]" . font-lock-string-face)
                       ("\\?.*\\?" . font-lock-negation-char-face)
                       ("\\[\\|\\]\\|{\\|}\\|(\\|)\\||\\|,\\|;" . font-lock-type-face)
                       ("[^ \t\n]" . font-lock-function-name-face))
                     '("\\.ebnf\\'")
                     `(
                       ,(lambda ()
                          (setq mode-name "EBNF")))
                     "Major mode for EBNF metasyntax text highlighting.")

(provide 'my/ebnf-mode)
;;; ebnf-mode.el ends here
