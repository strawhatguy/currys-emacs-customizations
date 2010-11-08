;;;; erlang mode
(let ((erl-path (executable-find "erl")))
  (when erl-path
    (let ((tp (shell-command-to-string
	       (concat erl-path " -noinput -eval \"io:format(\\\"~s\\\", [code:lib_dir(tools)])\" -run init stop"))))
      (setq load-path (cons (concat tp "/emacs")
			    load-path))
      (require 'erlang-start))))

(provide 'currys-erlang)
