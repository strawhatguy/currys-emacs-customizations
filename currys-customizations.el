;;;; Curry's customizations for emacs
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

;;;; Make meta ! always do an async shell command
;;;; async-shell-command copied from new simple.el in emacs repository
;;;; guard with fboundp
(unless (fboundp 'async-shell-command)
  (defun async-shell-command (command &optional output-buffer error-buffer)
    "Execute string COMMAND asynchronously in background.

Like `shell-command' but if COMMAND doesn't end in ampersand, adds `&'
surrounded by whitespace and executes the command asynchronously.
The output appears in the buffer `*Async Shell Command*'."
    (interactive
     (list
      (read-shell-command "Async shell command: " nil nil
			  (and buffer-file-name
			       (file-relative-name buffer-file-name)))
      current-prefix-arg
      shell-command-default-error-buffer))
    (unless (string-match "&[ \t]*\\'" command)
      (setq command (concat command " &")))
    (shell-command command output-buffer error-buffer)))

;;;; Remap shortcuts to use async-shell-command by default
(global-set-key [(meta !)] 'async-shell-command)
(global-set-key [(control meta !)] 'shell-command)

;;;; Add tab completion to shell-command mode
(require 'shell-command)
(shell-command-completion-mode)

;;;; Add ido-mode, for buffer-switching only
(require 'ido)
(ido-mode 'buffer)
(setq ido-enable-flex-matching t) ;; enable fuzzy matching

;;;; different way of uniquifying names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-after-kill-buffer-p nil)
(setq uniquify-ignore-buffers-re "^\\*")

;;;; SVN mode
(require 'psvn)

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


;;;; Buffer menu mode sort function
(defun buffer-list-sort (column)
  (interactive "SColumn to sort by (one of name,size,mode,file,time [default=time]): ")
  (case column
    (name (Buffer-menu-sort 2))
    (size (Buffer-menu-sort 3))
    (mode (Buffer-menu-sort 4))
    (file (Buffer-menu-sort 5))
    (t    (Buffer-menu-sort nil))))
(add-hook 'Buffer-menu-mode-hook
	  (lambda ()
	    (define-key Buffer-menu-mode-map (kbd "S") 'buffer-list-sort)))

;;;; For work, punch an e3 user proxy hole through a stack's fw
(defun setup-stack-tramp-proxies (stack)
  (interactive "sHostname of stack, NO FQDN (example: clay): ")
  (setq tramp-default-proxies-alist nil)
  (add-to-list 'tramp-default-proxies-alist
	       (list "\\.example\\.com"
		     nil 
		     (concat "/ssh:" stack ".skarven.net:"))))

;;;; pretty-print to a pdf file, with colorization of code
(defun print-to-pdf ()
  (interactive)
  (ps-spool-buffer-with-faces)
  (switch-to-buffer "*PostScript*")
  (write-file "/tmp/tmp.ps")
  (kill-buffer "tmp.ps")
  (setq cmd (concat "ps2pdf14 /tmp/tmp.ps " (buffer-name) ".pdf"))
  (shell-command cmd)
  (shell-command "rm /tmp/tmp.ps")
  (message (concat "Saved to:  " (buffer-name) ".pdf")))

;;;; set the scheme program to use
(setq scheme-program-name "mzscheme")

;;;; erlang mode
(let ((tp (shell-command-to-string "erl -noinput -eval \"io:format(\\\"~s\\\", [code:lib_dir(tools)])\" -run init stop")))
  (setq load-path (cons (concat tp "/emacs")
                        load-path))
  (require 'erlang-start))

;;;; js2-mode for javascript
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(setq js2-basic-offset 2)
(setq js2-bounce-indent-p t)

;;;; Load and setup slime
(require 'slime)
(slime-setup '(slime-repl))
;;;; Add ecl to slime (but doesn't seem to work right now)
(add-to-list 'slime-lisp-implementations
	     '(ecl ("ecl")))
;;;; Add sbcl to slime (last, so it's the default)
(setq inferior-lisp-program "sbcl")
(add-to-list 'slime-lisp-implementations
	     '(sbcl ("sbcl") :coding-system utf-8-unix))

;;;; CL helpers
;;;; Makes a new lisp package, see doc string
(defun new-lisp-package (name)
  "Make three files, <name>.lisp, package.lisp, and <name>.asd,
   which is a rudimentary skeleton for a Common Lisp ASDF package
   and system definition"
  (interactive "sName of package: ")
  (when (stringp name)
    (let ((oldbuf (current-buffer))
          (asd  (concat name ".asd"))
          (file (concat name ".lisp"))
          (pack "package.lisp"))
      (when (file-exists-p asd)  (error "File %s already exists!" asd))
      (when (file-exists-p file) (error "File %s already exists!" file))
      (when (file-exists-p pack) (error "File %s already exists!" pack))
      (find-file asd)
      (goto-char 0)
      (insert ";;;; -*-Lisp-*- " asd "\n")
      (insert "(in-package :cl-user)\n\n")
      (insert "(defpackage :" name "-asd (:use :cl))\n")
      (insert "(in-package :" name "-asd)\n\n")
      (insert "(asdf:defsystem :" name "\n"
              "  :version \"0.0.1\"\n"
              "  :serial t\n"
	      "  ;; external dependencies go here\n"
              "  :depends-on (:alexandria\n"
	      "               :cl-fad)\n"
              "  :components \n"
	      "   ((:file \"" (file-name-sans-extension pack) "\")\n"
	      "    (:file \"" (file-name-sans-extension file) "\")))\n")
      (lisp-mode)
      (font-lock-mode)
      (save-buffer (current-buffer))
      (find-file pack)
      (goto-char 0)
      (insert ";;;; " pack "\n\n")
      (insert "(in-package :common-lisp-user)\n\n")
      (insert "(defpackage :" name "\n")
      (insert "  (:nicknames :" name ")\n")
      (insert "  ;; insert other packages below\n")
      (insert "  (:use :cl)\n")
      (insert "  ;; export symbols here\n")
      (insert "  (:export    ))\n\n")
      (save-buffer (current-buffer))
      (find-file file)
      (goto-char 0)
      (insert ";;;; " file "\n\n")
      (insert "(in-package :" name ")\n\n")
      (save-buffer (current-buffer)))))


(provide 'currys-customizations)