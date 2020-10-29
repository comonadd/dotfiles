;; fonts
(setq doom-font (font-spec :family "Source Code Pro" :size 14)
      doom-big-font (font-spec :family "Source Code Pro" :size 24)
      doom-variable-pitch-font (font-spec :family "Source Code Pro" :size 14)
      doom-serif-font (font-spec :family "IBM Plex Mono" :weight 'light))
(setq doom-theme 'doom-vibrant)
(delq! t custom-theme-load-path)

;; Line numbers
(setq display-line-numbers-type 'relative)

;; Modeline
(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange"))

(custom-set-faces! '(doom-modeline-evil-insert-state :weight bold :foreground "#339CDB"))
(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

;; Borders
(set-frame-parameter (selected-frame) 'internal-border-width 0)
(setq ivy-posframe-border-width 0)

;; Theme
(setq doom-theme 'whatever)

;; Other
(setq-default x-stretch-cursor t) ; Stretch cursor to the glyph width
