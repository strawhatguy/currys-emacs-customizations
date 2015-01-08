;;;; -*- mode: emacs-lisp -*-
(require 'package)
(add-to-list 'package-archives 
             '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(
                      starter-kit
                      starter-kit-eshell
                      starter-kit-bindings
                      auto-complete
                      ac-slime
                      ac-helm
                      zenburn-theme 
                      dsvn
                      clojure-mode
                      nrepl
                      haskell-mode
                      ruby-compilation
		      shoulda
                      markdown-mode
                      yaml-mode
                      tuareg
                      marmalade
                      oddmuse
                      js2-mode
                      twittering-mode
                      jabber
                      yasnippet
                      ace-jump-mode
                      multiple-cursors
                      expand-region
                      wgrep
                      magit-svn
                      puppet-mode
                      vkill
                      google-this
                      google-maps
                      google-translate
                      nodejs-repl
                      coffee-mode
                      elnode
		      julia-mode
                      ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path (concat user-emacs-directory "include/"))

(load-theme 'zenburn t)

;;;; Darwin fixes
(when (eq system-type 'darwin)
  (setq ns-function-modifier 'hyper)
  (menu-bar-mode 1)
  (setenv "LANG" "en_US.UTF-8")
  (setq dired-use-ls-dired nil)
  (setq exec-path (append exec-path '("/usr/local/bin")))
  (setenv "PATH" "/Users/mjcurry/bin:/opt/local/libexec/gnubin:/usr/local/bin:/opt/local/bin:/opt/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin"))

;;;; make unlimited (was cutting off function names, which breaks things
(setq imenu-max-item-length t)

;;;; Shut off auto fill mode
(remove-hook 'text-mode-hook 'turn-on-auto-fill)

;;;; ace-jump
(require 'ace-jump-mode)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;;;; multiple-cursor mode
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-more-like-this-extended)

;;;; expand-region
(require 'expand-region)
(global-set-key (kbd "s-=") 'er/expand-region)
(global-set-key (kbd "s--") 'er/contract-region)

;;;; magit-status binding
(require 'magit)
(global-set-key (kbd "C-c g") 'magit-status)
(require 'magit-svn)

;;;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;;;; jabber
(eval-after-load 'jabber
  (progn
    (setq jabber-account-list (quote (("mcurry@im.skarven.net" (:port . 5223) (:connection-type . ssl)))))
    (setq jabber-autoaway-method nil)
    (setq jabber-chat-foreign-prompt-format "[%t] %n> 
        ")
    (setq jabber-chat-local-prompt-format "[%t] %n>
        ")
    (setq jabber-roster-show-title nil)))

;;;; toggle horizontal/vertical splitting
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))
(global-set-key [f9]   'toggle-window-split)

;;;; writable grep buffers via toggling off read-only (similar to
;;;; wdired mode for dired buffers)
(require 'wgrep)
(define-key grep-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode)
(setq wgrep-auto-save-buffer t)

;;;; Reset yasnippet trigger key
(require 'yasnippet)
(setq yas/trigger-key (kbd "C-c <kp-multiply>"))
(define-key yas/keymap (kbd "TAB") nil)
(yas-global-mode 1)

(add-to-list 'yas/root-directory 
             (car (file-expand-wildcards
                   "~/.emacs.d/elpa/yasnippet-*/snippets")))
(mkdir "~/.emacs.d/snippets" t)
(add-to-list 'yas/root-directory "~/.emacs.d/snippets")
(yas/reload-all)

;;;; auto-complete-mode
(require 'auto-complete-config)
(global-set-key (kbd "C-:") 'ac-complete-with-helm)
(define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-helm)
(add-to-list 'ac-dictionary-directories 
             (expand-file-name "~/.ac-dict"))
;;;; ac-common-setup is called by ac-config-default
(defun ac-common-setup ()
  (add-to-list 'ac-sources 'ac-source-yasnippet))
(add-to-list 'ac-modes 'html-mode)
(add-to-list 'ac-modes 'nxml-mode)
(ac-config-default)
(defun enable-auto-complete-mode ()
  (auto-complete-mode 1))
(defun disable-auto-complete-mode ()
  (auto-complete-mode 0))

;;;; Stop making backup files
(setq make-backup-files nil)

;;;; Make control tab switch buffer windows
(global-set-key [(control tab)] 'other-window)
(defun other-window-backwards (&optional count all-frames)
  (interactive)
  (other-window (if count (- count) -1) all-frames))
(global-set-key [(control shift iso-lefttab)] 'other-window-backwards)

;;;; Make meta g to goto-line
(global-set-key [(meta g)] 'goto-line)

;;;; set F5 key to revert-buffer
(defun reset-buffer () 
  "Resets a file-buffer reflect the file on disk, resetting modes"
  (interactive) (revert-buffer nil t nil))
(global-set-key [f5] 'reset-buffer)

;;;; set f8 to be recompile, shift-f8 to compile, scroll compile buffer
(defun compile-asking-directory (top-level)
  (interactive "DProject toplevel directory: ")
  (let ((default-directory top-level))
    (call-interactively 'compile)))
(global-set-key [f8]   'recompile)
(global-set-key [S-f8] 'compile-asking-directory)
(global-set-key [C-S-f8] 'compile)
(setq compilation-scroll-output t)

;;;; allow color in compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;;; Make C-h C-s go to apropos (basically apropos-symbol)
(global-set-key [(control h) (control s)] 'apropos)

;;;; Advise the shell commands to name the buffer after the command itself
(defadvice async-shell-command (before buffer-named-with-command
                                       (command &optional output-buffer error-buffer)
                                       activate compile)
  (setq output-buffer (or output-buffer (concat "*Async: " command "*")))
  (let ((dir default-directory))
    (switch-to-buffer output-buffer)
    (setq default-directory dir)))

(defadvice shell-command (before buffer-named-with-command
                                 (command &optional output-buffer error-buffer)
                                 activate compile)
  (setq output-buffer (or output-buffer (concat "*Shell: " command "*")))
  (let ((dir default-directory))
    (switch-to-buffer output-buffer)
    (setq default-directory dir)))

;;;; Remap shortcuts to use async-shell-command by default
(global-set-key [(meta !)] 'async-shell-command)
(global-set-key [(control meta !)] 'shell-command)
;;;; dired mode added it's own key, ensure it is gone.
(require 'dired)
(define-key dired-mode-map [(meta !)] nil)

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
(when (locate-library "edit-server")
  (require 'edit-server)
  (edit-server-start))

;;;; Add an alias to eshell to fork off processes
(defun eshell/fork (&rest args)
  (lexical-let ((cmd ""))
    (dolist (arg args)
      (setf cmd (concat cmd " " arg)))
    (setf cmd (subseq cmd 1))
    (async-shell-command cmd)))

;;;; Some rcirc mode configuration
(setq rcirc-default-full-name "Matthew Curry")

;;;; Twittering mode
(setq twittering-use-master-password t)

;;;; shoulda
(require 'shoulda)
(define-key ruby-mode-map (kbd "C-c s") 'shoulda-run-should-at-point)

;;;; js2 indent 2 spaces
(add-to-list 'auto-mode-alist '("\\.js\\'"   . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))
(set-default 'js2-basic-offset 2)
(set-default 'js2-mirror-mode  nil)
(set-default 'js2-mode-escape-quotes  nil)
(define-key js2-mode-map (kbd "C-M-p") 
  (lambda () 
    (interactive) 
    (js2-beginning-of-defun)))
(define-key js2-mode-map (kbd "C-M-n") 
  (lambda () 
    (interactive) 
    (js2-end-of-defun)))
(define-key js2-mode-map (kbd "C-c m") 'js2-mark-defun)

;;;; Enable paredit-mode for all lisps, disable paredit's C-j
(require 'paredit)
(define-key paredit-mode-map (kbd "C-j") nil)
(define-key paredit-mode-map (kbd "C-M-n") 'paredit-forward)
(define-key paredit-mode-map (kbd "C-M-p") 'paredit-backward)
(define-key paredit-mode-map (kbd "C-M-f") 'paredit-forward-up)
(define-key paredit-mode-map (kbd "C-M-b") 'paredit-backward-down)
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

;;;; scheme program, use chicken
(setq scheme-program-name "csi")

;;;; chicken-scheme setup
(require 'chicken-scheme)
(add-hook 'scheme-mode-hook 'setup-chicken-scheme)
(define-key scheme-mode-map (kbd "C-?") 'chicken-show-help)

;;;; Load and setup slime
(let ((slime-helper (expand-file-name "~/quicklisp/slime-helper.el")))
  (when (file-exists-p slime-helper)
    (load slime-helper)
    (setq slime-net-coding-system 'utf-8-unix)
    (setq slime-lisp-implementations
          '((sbcl ("/usr/local/bin/sbcl") :coding-system utf-8-unix)
            (ecl ("/usr/local/bin/ecl"))))

    ;; slime-connect to stumpwm
    (defun stumpwm-connect ()
      "slime-connect to stumpwm" 
      (interactive)
      (slime-connect "127.0.0.1" 5004))

    ;;;; Enable slime completion
    (require 'ac-slime)
    (add-hook 'slime-mode-hook 'set-up-slime-ac)
    (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
    (add-to-list 'ac-modes 'slime-repl-mode)
    (add-hook 'slime-mode-hook 
              (lambda () (define-key slime-mode-map [(tab)] 
                      'slime-indent-and-complete-symbol)))
    (add-hook 'slime-repl-mode-hook 
              (lambda () (define-key slime-repl-mode-map [(tab)] 
                      'slime-indent-and-complete-symbol)))

    ;;;; Hook in paredit to slime
    (add-hook 'slime-mode-hook      'enable-paredit-mode)
    (add-hook 'slime-repl-mode-hook 'enable-paredit-mode)))
