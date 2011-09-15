
;;;; emacs progess meter for mini-buffer example
(defun collect-mana-for-emacs ()
  (interactive)
  (let ((progress-reporter
	 (make-progress-reporter "Collecting mana for Emacs..."
				 0 500)))  
    (dotimes (k 500)
      (sit-for 0.01)
      (progress-reporter-update progress-reporter k))
    (progress-reporter-done progress-reporter)))
