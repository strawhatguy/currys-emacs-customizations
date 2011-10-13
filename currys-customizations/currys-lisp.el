;; ;;;; Load and setup slime
(load (expand-file-name "~/.quicklisp/slime-helper.el"))

(setq slime-net-coding-system 'utf-8-unix)
(setq slime-lisp-implementations
      '((sbcl ("/usr/local/bin/sbcl") :coding-system utf-8-unix)
        (ecl ("ecl"))))

;;;; Add per lisp hooks to turn on smart-tab-mode
(add-to-list 'smart-tab-completion-functions-alist 
             '(lisp-mode . slime-complete-symbol))
(add-to-list 'smart-tab-completion-functions-alist 
             '(common-lisp-mode . slime-complete-symbol))

(add-hook 'lisp-mode-hook 'smart-tab-mode-on)
(add-hook 'common-lisp-mode-hook 'smart-tab-mode-on)

;;;; CL helpers
;;;; Makes a new lisp package, see doc string
(defun new-lisp-package (name)
  "Make three files, <name>.lisp, package.lisp, and <name>.asd,
   which is a rudimentary skeleton for a Common Lisp ASDF package
   and system definition"
  (interactive "sName of package: ")
  (when (stringp name)
    (flet ((basename (filename &optional (sep "/"))
             (let ((name (copy-sequence filename)))
               (do ((index (string-match sep name) (string-match sep name)))
                   ((null index) name)
                 (setq name (subseq name (1+ index))))))
           (bail-on-file-existance (file)
             (when (file-exists-p file)
               (error "File %s already exists!" file))
             file)
           (make-asd-file (filename name dir first-file &rest files)
             (save-current-buffer
               (find-file filename)
               (insert ";;;; -*-Lisp-*- \n")
               (insert "(in-package :cl-user)\n\n")
               (insert "(defpackage :" name "-system (:use :cl))\n")
               (insert "(in-package :" name "-system)\n\n")
               (insert "(asdf:defsystem :" name "\n"
                       "  ;; external dependencies go here\n"
                       "  :depends-on ()\n"
                       "  :components \n"
                       "   ((:module \"" dir "\" \n"
                       "             :serial t\n"
                       "             :components \n")
               (insert "              ((:file \"" (file-name-sans-extension first-file) "\")")
               (dolist (file files)
                 (insert "\n               (:file \"" (file-name-sans-extension file) "\")"))
               (insert "))))\n\n")
               (lisp-mode)
               (font-lock-mode)
               (save-buffer (current-buffer))))
           (make-lisp-package-file (filename name)
             (save-current-buffer
               (find-file filename)
               (insert ";;;; -*-Lisp-*-\n")
               (insert "(in-package :common-lisp-user)\n\n")
               (insert "(defpackage :" name "\n")
               (insert "  ;; insert other packages below\n")
               (insert "  (:use :cl)\n")
               (insert "  ;; export symbols here\n")
               (insert "  (:export    ))\n\n")
               (save-buffer (current-buffer))))
           (make-lisp-file (filename name)
             (save-current-buffer
               (find-file filename)
               (insert ";;;; -*-Lisp-*-\n")
               (insert "(in-package :" name ")\n\n")
               (save-buffer (current-buffer)))))
      
      (mkdir "src" t)
      (mkdir "test" t)
      (let ((oldbuf (current-buffer))
            (test-name (concat name "-test"))
            (test-asd (bail-on-file-existance (concat name "-test.asd")))
            (asd  (bail-on-file-existance (concat name ".asd")))
            (pack (bail-on-file-existance (concat "src/"  "package.lisp")))
            (tpack (bail-on-file-existance (concat "test/"  "package.lisp")))
            (file (bail-on-file-existance (concat "src/" name ".lisp")))
            (test (bail-on-file-existance (concat "test/" name ".lisp"))))
        (make-asd-file asd name "src" (basename pack) (basename file))
        (make-asd-file test-asd test-name "test" (basename tpack) (basename test))
        (make-lisp-package-file pack name)
        (make-lisp-package-file tpack test-name)
        (make-lisp-file file name)
        (make-lisp-file test test-name)))))
  
(provide 'currys-lisp)
