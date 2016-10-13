(split-window-horizontally)
(transient-mark-mode 1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq package-enable-at-startup nil)
(package-initialize)

(tool-bar-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode 1)
(scroll-bar-mode -1)
(delete-selection-mode 1)

(set-fringe-mode 0)
(set-face-background 'hl-line "midnight blue")

; Defining variables BEGIN
(setq undo-limit 8192)
(setq undo-strong-limit 8192)
(setq shift-select-mode t)
(setq enable-local-variables nil)
(setq cua-auto-tabify-rectangles nil)
(setq cua-keep-region-after-copy t)
(setq auto-save-default nil)
(setq auto-save-list-file-name nil)
(setq make-backup-files nil)
; Defining variables END


; CC-Mode Plugin BEGIN
(require 'cc-mode)
; CC-Mode Plugin END

; Ido Plugin BEGIN
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
; Ido Plugin END

; Bs Plugin BEGIN
(require 'bs)
; Bs Plugin END

; Auto Complete Plugin BEGIN
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
; Auto Complete Plugin END

; Defining keys BEGIN
(define-key global-map (kbd "<f1>") 'bookmark-set)
(define-key global-map (kbd "<f2>") 'bookmark-jump)
(define-key global-map (kbd "<f3>") 'bookmark-bmenu-list)
(define-key global-map (kbd "<f9>") 'hs-hide-block)
(define-key global-map (kbd "C-<f9>") 'hs-show-block)
(define-key global-map (kbd "S-<f9>") 'hs-hide-all)
(define-key global-map (kbd "S-C-<f9>") 'hs-show-all)
(define-key global-map (kbd "<f12>") 'eval-last-sexp)
(define-key global-map (kbd "C-x") 'kill-region)
(define-key global-map (kbd "C-c") 'kill-ring-save)
(define-key global-map (kbd "C-v") 'yank)
(define-key global-map (kbd "C-d") 'kill-line)
(define-key global-map (kbd "C-z") 'undo)
(define-key global-map (kbd "C-s") 'save-buffer)
(define-key global-map (kbd "C-f") 'find-file)
(define-key global-map (kbd "C-a") 'mark-whole-buffer)
(define-key global-map (kbd "C-w") 'kill-this-buffer)
(define-key global-map (kbd "C-b") 'bs-show)
(define-key global-map (kbd "C-q") 'save-buffers-kill-terminal)
(define-key global-map (kbd "C-r") 'query-replace)
(define-key global-map (kbd "C-e") 'isearch-forward)
(define-key global-map (kbd "C-S-e") 'isearch-repeat-forward)
(define-key global-map (kbd "C-S-s") 'save-some-buffers)
(define-key global-map (kbd "C-S-f") 'find-file-other-window)
(define-key global-map (kbd "<C-tab>") 'other-window)
(define-key global-map (kbd "<backtab>") 'indent-for-tab-command)
; Defining keys END

; Defining functions BEGIN
(defun my:ac-c-headers-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories '"/usr/include/c++/5"))
; Defining functions END

; Adding hooks BEGIN
(add-hook 'c++-mode-hook 'my:ac-c-headers-init)
(add-hook 'c-mode-hook 'my:ac-c-headers-init)
(add-hook 'c++-mode-hook 'hs-minor-mode)
(add-hook 'c-mode-hook 'hs-minor-mode)
; Adding hooks END

; Indentation BEGIN
(setq-default c-basic-offset 4)
(setq c-default-style "linux"
      c-basic-offset 4)
; Indentation END

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("ad950f1b1bf65682e390f3547d479fd35d8c66cafa2b8aa28179d78122faa947" "12b4427ae6e0eef8b870b450e59e75122d5080016a9061c9696959e50d578057" "6df30cfb75df80e5808ac1557d5cc728746c8dbc9bc726de35b15180fa6e0ad9" "4f5bb895d88b6fe6a983e63429f154b8d939b4a8c581956493783b2515e22d6d" "74278d14b7d5cf691c4d846a4bbf6e62d32104986f104c1e61f718f9669ec04b" "3b903d4b0a528b92e7ec980568a47d47ee0cb3c56acd156340757817e2ddf1e5" "6bf237d23440fb0b340f4336695f2a08c6b785aa98288b3313526e76c38bca19" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "72c530c9c8f3561b5ab3bf5cda948cd917de23f48d9825b7a781fe1c0d737f2f" "0973b33d2f15e6eaf88400eee3dc8357ad8ae83d2ca43c125339b25850773a70" "ff6a8955945028387ed1a2b0338580274609fbb0d40cd011b98ca06bd00d9233" "daeaa8249f0c275de9e32ed822e82ff40457dabe07347fe06afc67d962a3b1e9" "b48599e24e6db1ea612061252e71abc2c05c05ac4b6ad532ad99ee085c7961a7" "c51e302edfe6d2effca9f7c9a8a8cfc432727efcf86246002a3b45e290306c1f" "a5a2954608aac5c4dcf9659c07132eaf0da25a8f298498a7eacf97e2adb71765" "68b847fac07094724e552eeaf96fa4c7e20824ed5f3f225cad871b8609d50ace" "1342a81078bdac27f80b86807b19cb27addc1f9e4c6a637a505ae3ba4699f777" "44f5578eccb2cde3b196dfa86a298b75fe39ceff975110c091fa8c874c338b50" "2ea9afebc23cca3cd0cd39943b8297ce059e31cb62302568b8fa5c25a22db5bc" "f19d195fa336e9904303eea20aad35036b79cfde72fa6e76b7462706acd52920" "bce1c321471d37b875f99c83cb7b451fd8386001259e1c0909d6e078ea60f00b" "45482e7ddf47ab1f30fe05f75e5f2d2118635f5797687e88571842ff6f18b4d5" "938f120eeda938eef2c36b4cc9609d1ad91b3a3666cd63a4be5b70b739004942" "53de65a1e7300e0f1a4f8bf317530a5008e9d06a0e2f8863b80dc56d77f844cf" "a621dd9749f2651e357a61f8d8d2d16fb6cacde3b3784d02151952e1b9781f05" "880f541eabc8c272d88e6a1d8917fe743552f17cedd8f138fe85987ee036ad08" "76935a29af65f8c915b1b3b4f6326e2e8d514ca098bd7db65b0caa533979fc01" "62a6731c3400093b092b3837cff1cb7d727a7f53059133f42fcc57846cfa0350" "0f302165235625ca5a827ac2f963c102a635f27879637d9021c04d845a32c568" "2047464bf6781156ebdac9e38a17b97bd2594b39cfeaab561afffcbbe19314e2" "aae40caa1c4f1662f7cae1ebfbcbb5aa8cf53558c81f5bc15baefaa2d8da0241" "aaf783d4bfae32af3e87102c456fba8a85b79f6e586f9911795ea79055dee3bf" "c39142cd89505a1b05130b65c85aed93e5a46426424a9143214cdb1778dbc8ce" "ec0c9d1715065a594af90e19e596e737c7b2cdaa18eb1b71baf7ef696adbefb0" "5c5de678730ceb4e05794431dd65f30ffe9f1ed6c016fa766cdf909ba03e4df4" "b6f06081b007b57be61b82fb53f27315e2cf38fa690be50d6d63d2b62a408636" "995d0754b79c4940d82bd430d7ebecca701a08631ec46ddcd2c9557059758d33" "70b2d5330a8dd506accac4b51aaa7e43039503d000852d7d152aec2ce779d96d" "011d4421eedbf1a871d1a1b3a4d61f4d0a2be516d4c94e111dfbdc121da0b043" "57d7e8b7b7e0a22dc07357f0c30d18b33ffcbb7bcd9013ab2c9f70748cfa4838" "d9e811d5a12dec79289c5bacaecd8ae393d168e9a92a659542c2a9bab6102041" "b4fd44f653c69fb95d3f34f071b223ae705bb691fb9abaf2ffca3351e92aa374" "09feeb867d1ca5c1a33050d857ad6a5d62ad888f4b9136ec42002d6cdf310235" "9a3c51c59edfefd53e5de64c9da248c24b628d4e78cc808611abd15b3e58858f" "b8c5adfc0230bd8e8d73450c2cd4044ad7ba1d24458e37b6dec65607fc392980" "67b11ee5d10f1b5f7638035d1a38f77bca5797b5f5b21d16a20b5f0452cbeb46" "3fe4861111710e42230627f38ebb8f966391eadefb8b809f4bfb8340a4e85529" "5c83b15581cb7274085ba9e486933062652091b389f4080e94e4e9661eaab1aa" "da8e6e5b286cbcec4a1a99f273a466de34763eefd0e84a41c71543b16cd2efac" "77515a438dc348e9d32310c070bfdddc5605efc83671a159b223e89044e4c4f1" "a513bb141af8ece2400daf32251d7afa7813b3a463072020bb14c82fd3a5fe30" "9bd5ee2b24759fbc97f86c2783d1bf8f883eb1c0dd2cf7bda2b539cd28abf6a9" "28818b9b1d9e58c4fb90825a1b07b0f38286a7d60bf0499bc2dea7eea7e36782" "cb30d82b05359203c8378638dec5ad6e37333ccdda9dee8b9fdf0c902e83fad7" "6b4f7bde1ce64ea4604819fe56ff12cda2a8c803703b677fdfdb603e8b1f8bcb" "01e0367d8c3249928a2e0ebc9807b2f791f81a0d2a7c8656e1fbf4b1dbaa404c" "6291d73aaeb6a3d7e455d85ca3865260a87afe5f492b6d0e2e391af2d3ea81dd" "6c0d748fb584ec4c8a0a1c05ce1ae8cde46faff5587e6de1cc59d3fc6618e164" "335ad769bcd7949d372879ec10105245255beec6e62213213835651e2eb0b8e0" "4bcdfc98cf64ce6145684dc8288fd87489cfa839e07f95f6c791d407624d04f8" "31772cd378fd8267d6427cec2d02d599eee14a1b60e9b2b894dd5487bd30978e" "8e7044bfad5a2e70dfc4671337a4f772ee1b41c5677b8318f17f046faa42b16b" "b5cff93c3c6ed12d09ce827231b0f5d4925cfda018c9dcf93a2517ce3739e7f1" "63aff36a40f41b28b0265ac506faa098fd552fac0a1813b745ba7c27efa5a943" "d12c2cae6c13a834084e06a3062d5a27cac7627e0872bd1728d203b46ae6a5bb" "7dad2be7d806486305d7d3afe6b53a0c882cf651e183ed1ffe6dfb0745dc80f6" "deb7ae3a735635a85c984ece4ce70317268df6027286998b0ea3d10f00764c9b" "abd7719fd9255fcd64f631664390e2eb89768a290ee082a9f0520c5f12a660a8" "1127f29b2e4e4324fe170038cbd5d0d713124588a93941b38e6295a58a48b24f" "fb09acc5f09e521581487697c75b71414830b1b0a2405c16a9ece41b2ae64222" "e26e879d250140e0d4c4d5ab457c32bcb29742599bd28c1ce31301344c6f2a11" "b71da830ae97a9b70d14348781494b6c1099dbbb9b1f51494c3dfa5097729736" "701b4b4e7989329a0704b92fc17e6600cc18f9df4f2466617ec91c932b5477eb" "6a674ffa24341f2f129793923d0b5f26d59a8891edd7d9330a258b58e767778a" "39a854967792547c704cbff8ad4f97429f77dfcf7b3b4d2a62679ecd34b608da" "2d5c40e709543f156d3dee750cd9ac580a20a371f1b1e1e3ecbef2b895cf0cd2" "392f19e7788de27faf128a6f56325123c47205f477da227baf6a6a918f73b5dc" "7bd626fcc9fbfb44186cf3f08b8055d5a15e748d5338e47f9391d459586e20db" "a455366c5cdacebd8adaa99d50e37430b0170326e7640a688e9d9ad406e2edfd" "a02c000c95c43a57fe1ed57b172b314465bd11085faf6152d151385065e0e4b1" "1a2cde373eff9ffd5679957c7ecfc6249d353e1ee446d104459e73e924fe0d8a" "3ed2e1653742e5059e3d77af013ee90c1c1b776d83ec33e1a9ead556c19c694b" "2ae4b0c50dd49a5f74edeae3e49965bf8853954b63c5712a7967ea0a008ecd5b" "1f126eb4a1e5d6b96b3faf494c8c490f1d1e5ad4fc5a1ce120034fe140e77b88" "cb39485fd94dabefc5f2b729b963cbd0bac9461000c57eae454131ed4954a8ac" "fc1137ae841a32f8be689e0cfa07c872df252d48426a47f70dba65f5b0f88ac4" "aad7fd3672aad03901bf91e338cd530b87efc2162697a6bef79d7f8281fd97e3" "fe349b21bb978bb1f1f2db05bc87b2c6d02f1a7fe3f27584cd7b6fbf8e53391a" "a11043406c7c4233bfd66498e83600f4109c83420714a2bd0cd131f81cbbacea" "780c67d3b58b524aa485a146ad9e837051918b722fd32fd1b7e50ec36d413e70" "9dc64d345811d74b5cd0dac92e5717e1016573417b23811b2c37bb985da41da2" "6cf0e8d082a890e94e4423fc9e222beefdbacee6210602524b7c84d207a5dfb5" "f831c1716ebc909abe3c851569a402782b01074e665a4c140e3e52214f7504a0" "595099e6f4a036d71de7e1512656e9375dd72cf60ff69a5f6d14f0171f1de9c1" "ed92c27d2d086496b232617213a4e4a28110bdc0730a9457edf74f81b782c5cf" "5eb4b22e97ddb2db9ecce7d983fa45eb8367447f151c7e1b033af27820f43760" "4c8372c68b3eab14516b6ab8233de2f9e0ecac01aaa859e547f902d27310c0c3" "8530b2f7b281ea6f263be265dd8c75b502ecd7a30b9a0f28fa9398739e833a35" "1a094b79734450a146b0c43afb6c669045d7a8a5c28bc0210aba28d36f85d86f" "05d009b7979e3887c917ef6796978d1c3bbe617e6aa791db38f05be713da0ba0" "6e03b7f86fcca5ce4e63cda5cd0da592973e30b5c5edf198eddf51db7a12b832" default)))
 '(ensime-sem-high-faces
   (quote
    ((var :foreground "#9876aa" :underline
	  (:style wave :color "yellow"))
     (val :foreground "#9876aa")
     (varField :slant italic)
     (valField :foreground "#9876aa" :slant italic)
     (functionCall :foreground "#a9b7c6")
     (implicitConversion :underline
			 (:color "#808080"))
     (implicitParams :underline
		     (:color "#808080"))
     (operator :foreground "#cc7832")
     (param :foreground "#a9b7c6")
     (class :foreground "#4e807d")
     (trait :foreground "#4e807d" :slant italic)
     (object :foreground "#6897bb" :slant italic)
     (package :foreground "#cc7832")
     (deprecated :strike-through "#a9b7c6"))))
 '(fci-rule-color "#49483E")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#49483E" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#49483E" . 100))))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/"))))
 '(pos-tip-background-color "#36473A")
 '(pos-tip-foreground-color "#FFFFC8")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
