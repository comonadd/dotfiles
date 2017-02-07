;;; auto-insert-skeletons.el --- auto-insert skeletons
;;; Commentary:
;;; Code:

;; Constants

(defvar author-info "Dmitry Guzeev <dmitry.guzeev@yahoo.com>")

;; "generate file top comment" functions

(defun gen-c-file-top-comment ()
  "Generate a C file top comment."
  (concat
   "/* File: " (buffer-file-name-nondir) " */\n"
   "/* Creation date: " (get-current-date) " */\n"
   "/* Creator: " author-info " */\n"
   "/* Description: */\n"))

(defun gen-lua-file-top-comment ()
  "Generate a Lua file top comment."
  (concat
   "-- File: " (buffer-file-name-nondir) "\n"
   "-- Creation date: " (get-current-date) "\n"
   "-- Creator: " author-info "\n"
   "-- Description:\n"))

(defun gen-elisp-file-top-comment ()
  "Generate a Emacs Lisp file top comment."
  (let ((file-name-nondir (buffer-file-name-nondir)))
    (concat
     ";;; " file-name-nondir " --- description\n"
     ";;; Commentary:\n"
     ";;; Code:\n\n\n"
     "(provide '" (buffer-get-base-file-name) ")\n"
     ";;; " file-name-nondir " ends here")))

;; "generate skeleton" functions

(defun gen-c-source-file-skeleton ()
  "Generate a skeleton for the C source file."
  (let ((base-file-name (buffer-get-base-file-name)))
    (concat
     (gen-c-file-top-comment)
     "\n#include \"" base-file-name ".h\"\n")))

(defun gen-c-header-file-skeleton ()
  "Generate a skeleton for the C header file."
  (let ((base-file-name (buffer-get-base-file-name)))
    (let ((upcase-base-file-name (upcase base-file-name)))
      (concat
       (gen-c-file-top-comment) "\n"
       "#ifndef " upcase-base-file-name "_H\n"
       "#define " upcase-base-file-name "_H\n\n\n\n"
       "#endif /* " upcase-base-file-name "_H */"))))

(defun gen-lua-file-skeleton ()
  "Generate a skeleton for the Lua file."
  (concat (gen-lua-file-top-comment) "\n"))

(defun gen-elisp-file-skeleton ()
  "Generate a skeleton for the Emacs Lisp file."
  (gen-elisp-file-top-comment))

;; Definition of skeletons

(define-auto-insert
  '("\\.c" . "C source file skeleton")
  '("Short description: "
    (gen-c-source-file-skeleton)))

(define-auto-insert
  '("\\.h" . "C header file skeleton")
  '("Short description: "
    (gen-c-header-file-skeleton)))

(define-auto-insert
  '("\\.lua" . "Lua file skeleton")
  '("Short description: "
    (gen-lua-file-skeleton)))

(define-auto-insert
  '("\\.el" . "Emacs Lisp file skeleton")
  '("Short description: "
    (gen-elisp-file-skeleton)))

(provide 'auto-insert-skeletons)
;;; auto-insert-skeletons.el ends here
