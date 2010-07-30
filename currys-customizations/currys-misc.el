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

;;;; Man-mode makes manpage active buffer
(setq Man-notify-method 'aggressive)

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


(provide 'currys-misc)