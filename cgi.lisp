
(in-package :twenty-forty-eight)

(defun cgi ()
  "Pick up crap from the environment and use it to make something
wonderful.  Recycling?"
  (let* ((env (http:http-parse-query-string))
	 (board (or (gethash "board" env)
                    "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 CPU")))
    (princ (html-board board))))
