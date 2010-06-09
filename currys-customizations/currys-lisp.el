
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
          (test (concat name "-test.lisp"))
          (test-asd (concat name "-test.asd"))
          (pack "package.lisp"))
      (when (file-exists-p test-asd) (error "File %s already exists!" test-asd))
      (when (file-exists-p asd)  (error "File %s already exists!" asd))
      (when (file-exists-p test) (error "File %s already exists!" test))
      (when (file-exists-p file) (error "File %s already exists!" file))
      (when (file-exists-p pack) (error "File %s already exists!" pack))

      (find-file asd)
      (goto-char 0)
      (insert ";;;; -*-Lisp-*- " asd "\n")
      (insert "(in-package :cl-user)\n\n")
      (insert "(defpackage :" name "-system (:use :cl))\n")
      (insert "(in-package :" name "-system)\n\n")
      (insert "(asdf:defsystem :" name "\n"
              "  :serial t\n"
	      "  ;; external dependencies go here\n"
              "  :depends-on (:alexandria)\n"
              "  :components \n"
	      "   ((:file \"" (file-name-sans-extension pack) "\")\n"
	      "    (:file \"" (file-name-sans-extension file) "\")))\n\n")
      (lisp-mode)
      (font-lock-mode)
      (save-buffer (current-buffer))

      (find-file test-asd)
      (goto-char 0)
      (insert ";;;; -*-Lisp-*- " test-asd "\n")
      (insert "(in-package :cl-user)\n\n")
      (insert "(defpackage :" name "-test-system (:use :cl))\n")
      (insert "(in-package :" name "-test-system)\n\n")
      (insert "(asdf:defsystem :" name "-test\n"
              "  :serial t\n"
	      "  ;; external dependencies go here\n"
              "  :depends-on (:alexandria\n"
	      "               :sb-rt\n"
	      "               :" name ")\n"
              "  :components \n"
	      "   ((:file \"" (file-name-sans-extension test) "\")))\n")
      (lisp-mode)
      (font-lock-mode)
      (save-buffer (current-buffer))

      (find-file pack)
      (goto-char 0)
      (insert ";;;; " pack "\n\n")
      (insert "(in-package :common-lisp-user)\n\n")
      (insert "(defpackage :" name "\n")
      (insert "  ;; insert other packages below\n")
      (insert "  (:use :cl :alexandria)\n")
      (insert "  ;; export symbols here\n")
      (insert "  (:export    ))\n\n")
      (save-buffer (current-buffer))

      (find-file file)
      (goto-char 0)
      (insert ";;;; " file "\n\n")
      (insert "(in-package :" name ")\n\n")
      (save-buffer (current-buffer))

      (find-file test)
      (goto-char 0)
      (insert ";;;; " test "\n\n")
      (insert "(require :sb-rt)\n")
      (insert "(defpackage :" name "-test\n")
      (insert "  (:use :cl :alexandria :sb-rt :" name "))\n\n")
      (insert "(in-package :" name "-test)\n\n")
      (save-buffer (current-buffer)))))

(provide 'currys-lisp)
