(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(require 'autopair)
(autopair-global-mode)

(require 'multiple-cursors)

(provide 'packages)
