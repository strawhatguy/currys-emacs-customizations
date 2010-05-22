
;;;; set the scheme program to use
(setq scheme-program-name "mzscheme")

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

(provide 'currys-lisp)
