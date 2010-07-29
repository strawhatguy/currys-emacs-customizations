
;;;; Disable bars
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;;;; Add line and column numbers to the modeline
(line-number-mode 1)
(column-number-mode 1)

;;;; Set to a usable font
(set-default-font "Liberation Mono-12")
;; (set-default-font "-*-liberation.mono-bold-r-*-*-17-*-*-*-*-*-*-*")
;; (set-default-font "-*-liberation.mono-bold-r-*-*-17-*-*-*-*-*-*-*")

;;;; Stop making backup files
(setq make-backup-files nil)

;;;; Make control tab switch buffer windows
(global-set-key [(control tab)] 'other-window)

;;;; Make meta g to goto-line
(global-set-key [(meta g)] 'goto-line)

;;;; Allow terminal colorization
(ansi-color-for-comint-mode-on)

;;;; Change color scheme to Zenburn default
(require 'color-theme)
(require 'zenburn)
(color-theme-zenburn)

;;;; Highlight entire expression within parens
(show-paren-mode t)
(setq show-paren-style 'expression)
(set-face-background 'show-paren-match-face "#1f3f3f")

;;;; Don't spawn a new frame for the ediff commands, keep it all in one frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;;;; Have ediff buffers show in a side-by-side view
(setq ediff-split-window-function 'split-window-horizontally)

;;;; smart-tab
(require 'smart-tab)

;;;; Set browser to chrome
(setq browse-url-generic-program (executable-find "google-chrome")
      browse-url-browser-function 'browse-url-generic)

(provide 'currys-theme)