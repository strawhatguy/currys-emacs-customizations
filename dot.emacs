;;;; -*- mode: emacs-lisp -*-
(require 'package)
(add-to-list 'package-archives 
             '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(
                      starter-kit
                      starter-kit-eshell
                      starter-kit-bindings
                      starter-kit-ruby
                      auto-complete 
                      zenburn-theme 
                      dsvn
                      clojure-mode
                      markdown-mode
                      yaml-mode
                      tuareg
                      marmalade
                      oddmuse
                      js2-mode
                      rinari
                      ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path (concat user-emacs-directory "include/"))

(load-theme 'zenburn t)

;;;; auto-complete-mode
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories 
             (expand-file-name "~/.ac-dict"))
(ac-config-default)
(defun enable-auto-complete-mode ()
  (auto-complete-mode 1))
(defun disable-auto-complete-mode ()
  (auto-complete-mode 0))

;;;; Stop making backup files
(setq make-backup-files nil)

;;;; Make control tab switch buffer windows
(global-set-key [(control tab)] 'other-window)

;;;; Make meta g to goto-line
(global-set-key [(meta g)] 'goto-line)

;;;; set F5 key to revert-buffer
(defun reset-buffer () 
  "Resets a file-buffer reflect the file on disk, resetting modes"
  (interactive) (revert-buffer nil t nil))
(global-set-key [f5] 'reset-buffer)

;;;; Make C-h C-s go to apropos (basically apropos-symbol)
(global-set-key [(control h) (control s)] 'apropos)

;;;; Remap shortcuts to use async-shell-command by default
(global-set-key [(meta !)] 'async-shell-command)
(global-set-key [(control meta !)] 'shell-command)

;;;; Buffer menu mode sort function
(defun buffer-list-sort (column)
  (interactive "SColumn to sort by (one of name,size,mode,file,time [default=time]): ")
  (case column
    (name (Buffer-menu-sort 2))
    (size (Buffer-menu-sort 3))
    (mode (Buffer-menu-sort 4))
    (file (Buffer-menu-sort 5))
    (t    (Buffer-menu-sort nil))))
(defun Buffer-mode-sort-key-hook ()
  (define-key Buffer-menu-mode-map (kbd "S") 'buffer-list-sort))
(add-hook 'Buffer-menu-mode-hook 'Buffer-mode-sort-key-hook)

;;;; Allow terminal colorization
(ansi-color-for-comint-mode-on)

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

;;;; edit-server for chromium browsers
(when (and (daemonp) (locate-library "edit-server"))
  (require 'edit-server)
  (edit-server-start))

;;;; Add an alias to eshell to fork off processes
(defun eshell/fork (&rest args)
  (lexical-let ((cmd ""))
    (dolist (arg args)
      (setf cmd (concat cmd " " arg)))
    (setf cmd (subseq cmd 1))
    (async-shell-command cmd (switch-to-buffer (concat "*Async: " cmd "*")))))

;;;; Some rcirc mode configuration
(setq rcirc-default-full-name "Matthew Curry")

;;;; Enable paredit-mode for all lisps, disable paredit's C-j
(require 'paredit)
(define-key paredit-mode-map "\C-j" nil)
(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
(add-hook 'ielm-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook           'enable-paredit-mode)
(add-hook 'inferior-scheme-mode-hook  'enable-paredit-mode)

;;;; Advice for ielm-mode
(defadvice ielm-eval-input (after ielm-paredit activate)
  "Begin each IELM prompt with a ParEdit parenthesis pair."
  (paredit-open-round))

;;;; scheme program, use guile
(require 'cmuscheme)
(setq scheme-program-name "guile")

;;;; Load and setup slime
(let ((slime-helper (expand-file-name "~/.quicklisp/slime-helper.el")))
  (when (file-exists-p slime-helper)
    (load slime-helper)
    (setq slime-net-coding-system 'utf-8-unix)
    (setq slime-lisp-implementations
          '((sbcl ("/usr/local/bin/sbcl") :coding-system utf-8-unix)
            (ecl ("ecl"))))
    ;;;; Enable slime completion
    (define-key slime-mode-map [(tab)] 'slime-indent-and-complete-symbol)
    (define-key slime-repl-mode-map [(tab)] 'slime-indent-and-complete-symbol)

    ;;;; Disable auto-complete-mode for slime
    (add-hook 'slime-mode-hook      'disable-auto-complete-mode)
    (add-hook 'slime-repl-mode-hook 'disable-auto-complete-mode)

    ;;;; Hook in paredit to slime
    (add-hook 'slime-mode-hook      'enable-paredit-mode)
    (add-hook 'slime-repl-mode-hook 'enable-paredit-mode)))
