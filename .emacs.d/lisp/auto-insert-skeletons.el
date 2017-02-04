;;; auto-insert-skeletons --- auto-insert skeletons
;;; Commentary:
;;; Code:

(require 'util)

(defun gen-c-file-top-comment ()
  "Generate a C file top comment."
  (concat
   "/* File : " (buffer-file-name-nondir) " */\n"
   "/* Creation date: " (get-current-date) " */\n"
   "/* Creator: Dmitry Guzeev <dmitry.guzeev@yahoo.com> */\n"
   "/* Description: */\n"))

(defun gen-c-source-file-skeleton ()
  "Generate a skeleton for the C source file."
  (let ((base-file-name (buffer-get-base-file-name)))
    (progn
      (concat
       (gen-c-file-top-comment)
       "\n#include \"" base-file-name ".h\"\n"))))

(defun gen-c-header-file-skeleton ()
  "Generate a skeleton for the C header file."
  (let ((base-file-name (buffer-get-base-file-name)))
    (let ((upcase-base-file-name (upcase base-file-name)))
      (progn
        (concat
         (gen-c-file-top-comment)
         "\n#ifndef " upcase-base-file-name "_H\n"
         "#define " upcase-base-file-name "_H\n\n\n\n"
         "#endif /* " upcase-base-file-name "_H */")))))

(define-auto-insert
  '("\\.c" . "C source file skeleton")
  '("Short description: "
    (gen-c-source-file-skeleton)))

(define-auto-insert
  '("\\.h" . "C header file skeleton")
  '("Short description: "
    (gen-c-header-file-skeleton)))

(provide 'auto-insert-skeletons)
;;; auto-insert-skeletons.el ends here
