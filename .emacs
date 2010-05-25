;;;; -*- Emacs-Lisp -*-

;;;; Hmm... never needed the cl package before...
(require 'cl)

;;;; get directories only one level deep, plus the directory itself
(defun get-subdirs-of-dir (dir)
  (lexical-let ((d (expand-file-name dir)))
    (cons d
	  (mapcar (lambda (f)
		    (concat d "/" (car f)))
		  (remove-if-not (lambda (file)
				   (and (cadr file)
					(not (equal "." (car file)))
					(not (equal ".." (car file)))))
				 (directory-files-and-attributes d))))))

;;;; Add a new paths to load emacs stuff
(mapc (lambda (path)
	(add-to-list 'load-path path))
      (get-subdirs-of-dir "REPLACEME"))

;;;; require my brand of emacs customizations
(require 'currys-theme)
(require 'currys-misc)
(require 'currys-erlang)
(require 'currys-javascript)
(require 'currys-lisp)
