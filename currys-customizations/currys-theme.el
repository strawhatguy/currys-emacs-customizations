
;;;; Disable bars
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;;;; Add line and column numbers to the modeline
(line-number-mode 1)
(column-number-mode 1)

;;;; Set to a usable font
(add-to-list 'default-frame-alist '(font-backend . "xft"))
(add-to-list 'default-frame-alist '(font . "Liberation Mono-12"))

;;;; Stop making backup files
(setq make-backup-files nil)

;;;; Make control tab switch buffer windows
(global-set-key [(control tab)] 'other-window)

;;;; Make meta g to goto-line
(global-set-key [(meta g)] 'goto-line)

;;;; set F5 key to revert-buffer
(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t nil)))

;;;; Make C-h C-s go to apropos (basically apropos-symbol)
(global-set-key [(control h) (control s)] 'apropos)

;;;; Allow terminal colorization
(ansi-color-for-comint-mode-on)

;;;; Change color scheme to Zenburn default
(require 'color-theme)
(require 'color-theme-zenburn)
(color-theme-zenburn)

;;;; Highlight entire expression within parens
(show-paren-mode t)
(setq show-paren-style 'expression)
(set-face-background 'show-paren-match-face "#1f3f3f")

;;;; Don't spawn a new frame for the ediff commands, keep it all in one frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;;;; Have ediff buffers show in a side-by-side view
(setq ediff-split-window-function 'split-window-horizontally)

;;;; Set browser
(dolist (executable (list "google-chrome" "chromium-browser" "firefox"))
  (let ((browser-path (executable-find executable)))
    (when browser-path
      (setq browse-url-generic-program browser-path
            browse-url-browser-function 'browse-url-generic)
      (return browser-path))))

(provide 'currys-theme)