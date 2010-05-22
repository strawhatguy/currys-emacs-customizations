;;;; erlang mode
(let ((tp (shell-command-to-string "erl -noinput -eval \"io:format(\\\"~s\\\", [code:lib_dir(tools)])\" -run init stop")))
  (setq load-path (cons (concat tp "/emacs")
                        load-path))
  (require 'erlang-start))

(provide 'currys-erlang)
