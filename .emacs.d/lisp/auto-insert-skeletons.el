;;; auto-insert-skeletons.el --- auto-insert skeletons
;;; Commentary:
;;; Code:

(require 'util)

(defvar author-info "Dmitry Guzeev <dmitry.guzeev@yahoo.com>")

;; "generate file top comment" functions

(defun gen-c-file-top-comment ()
  "Generate a C file top comment."
  (concat "/* File: " (buffer-file-name-nondir) " */\n" "/* Creation date: " (get-current-date)
          " */\n" "/* Creator: " author-info " */\n" "/* Description: */\n"))

(defun gen-lua-file-top-comment ()
  "Generate a Lua file top comment."
  (concat "-- File: " (buffer-file-name-nondir) "\n" "-- Creation date: " (get-current-date) "\n"
          "-- Creator: " author-info "\n" "-- Description:\n"))

(defun gen-elisp-file-top-comment ()
  "Generate a Emacs Lisp file top comment."
  (let ((file-name-nondir (buffer-file-name-nondir)))
    (concat ";;; " file-name-nondir " --- description\n" ";;; Commentary:\n" ";;; Code:\n\n\n"
            "(provide '" (buffer-get-base-file-name) ")\n" ";;; " file-name-nondir " ends here")))

(defun gen-sh-file-top-comment ()
  "Generate a SH file top comment."
  (concat "# File: " (buffer-file-name-nondir) "\n" "# Creation date: " (get-current-date) "\n"
          "# Creator: " author-info "\n" "# Description:\n"))

(defun gen-python-file-top-comment ()
  "Generate a Python file top comment."
  (gen-sh-file-top-comment))

(defun gen-make-file-top-comment ()
  "Generate a file top comment for the Make files."
  (gen-sh-file-top-comment))

(defun gen-cmake-file-top-comment ()
  "Generate a file top comment for the CMake file."
  (gen-sh-file-top-comment))

(defun gen-scss-file-top-comment ()
  "Generate a file top comment for the SCSS file."
  (gen-c-file-top-comment))

(defun gen-html-file-top-comment ()
  "Generate a file top comment for the HTML file."
  (concat "<!--\n"
          "  File: " (buffer-file-name-nondir) "\n"
          "  Creation date: " (get-current-date) "\n"
          "  Creator: " author-info "\n"
          "  Description:\n"
          "-->\n"))

(defun gen-js-file-top-comment ()
  "Generate a file top comment for the JavaScript file."
  (gen-c-file-top-comment))

;; "generate skeleton" functions

(defun gen-c-source-file-skeleton ()
  "Generate a skeleton for the C source file."
  (let ((base-file-name (buffer-get-base-file-name)))
    (concat (gen-c-file-top-comment) "\n#include \"" base-file-name ".h\"\n")))

(defun gen-c-header-file-skeleton ()
  "Generate a skeleton for the C header file."
  (let ((base-file-name (buffer-get-base-file-name)))
    (let ((upcase-base-file-name (upcase base-file-name)))
      (concat (gen-c-file-top-comment) "\n" "#ifndef " upcase-base-file-name "_H\n" "#define "
              upcase-base-file-name "_H\n\n\n\n" "#endif /* " upcase-base-file-name "_H */"))))

(defun gen-lua-file-skeleton ()
  "Generate a skeleton for the Lua file."
  (concat (gen-lua-file-top-comment) "\n"))

(defun gen-elisp-file-skeleton ()
  "Generate a skeleton for the Emacs Lisp file."
  (gen-elisp-file-top-comment))

(defun gen-sh-file-skeleton ()
  "Generate a skeleton for the Sh file."
  (gen-sh-file-top-comment))

(defun gen-python-file-skeleton ()
  "Generate a skeleton for the Python file."
  (gen-python-file-top-comment))

(defun gen-make-file-skeleton ()
  "Generate a skeleton for the Make file."
  (gen-make-file-top-comment))

(defun gen-cmake-file-skeleton ()
  "Generate a skeleton for the CMake file."
  (gen-cmake-file-top-comment))

(defun gen-scss-file-skeleton ()
  "Generate a skeleton for the SCSS file."
  (gen-scss-file-top-comment))

(defun gen-html-file-skeleton ()
  "Generate a skeleton for the HTML file."
  (gen-html-file-top-comment))

(defun gen-js-file-skeleton ()
  "Generate a skeleton for the JavaScript file."
  (gen-js-file-top-comment))

;; Definition of skeletons

(define-auto-insert '("\\.c" . "C source file skeleton")
  '("Short description: " (gen-c-source-file-skeleton)))

(define-auto-insert '("\\.h" . "C header file skeleton")
  '("Short description: " (gen-c-header-file-skeleton)))

(define-auto-insert '("\\.lua" . "Lua file skeleton")
  '("Short description: " (gen-lua-file-skeleton)))

(define-auto-insert '("\\.el" . "Emacs Lisp file skeleton")
  '("Short description: " (gen-elisp-file-skeleton)))

(define-auto-insert '("\\.sh" . "Sh file skeleton")
  '("Short description: " (gen-sh-file-skeleton)))

(define-auto-insert '("\\.py" . "Python file skeleton")
  '("Short description: " (gen-python-file-skeleton)))

(define-auto-insert '("\\.mk" . "Make file skeleton")
  '("Short description: " (gen-make-file-skeleton)))

(define-auto-insert '("Makefile" . "Make file skeleton")
  '("Short description: " (gen-make-file-skeleton)))

(define-auto-insert '("CMakeLists.txt" . "CMake file skeleton")
  '("Short description: " (gen-cmake-file-skeleton)))

(define-auto-insert '("\\.cmake" . "CMake file skeleton")
  '("Short description: " (gen-cmake-file-skeleton)))

(define-auto-insert '("\\.scss" . "SCSS file skeleton")
  '("Short description: " (gen-scss-file-skeleton)))

(define-auto-insert '("\\.html" . "HTML file skeleton")
  '("Short description: " (gen-html-file-skeleton)))

(define-auto-insert '("\\.js" . "JavaScript file skeleton")
  '("Short description: " (gen-js-file-skeleton)))

(provide 'auto-insert-skeletons)
;;; auto-insert-skeletons.el ends here
