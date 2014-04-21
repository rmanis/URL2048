
(in-package :twenty-forty-eight)

(defun cgi ()
  "Pick up crap from the environment and use it to make something
wonderful.  Recycling?"
  (let* ((env (http:http-parse-query-string))
	 (board (if env
                    (or (gethash "board" env)
                        "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 CPU")
                    "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 CPU"))
         (style (determine-style board)))
    (princ
     (case style
       (:spaces (html-board-spaces board))
       (:letters (html-board-letters board))))))
