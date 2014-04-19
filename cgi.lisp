
(in-package :twenty-forty-eight)

(defun cgi ()
  "Pick up crap from the environment and use it to make something
wonderful.  Recycling?"
  (let* ((env (http:http-parse-query-string))
	 (board (gethash "board" env)))
    (princ (html-board board))))
