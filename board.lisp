
(defun move (direction board-string)
  (print-board (move-board direction
			(read-board board-string))))

(defun move-board (direction board)
  (case direction
    (:left (left board))
    (:right (right board))
    (:up (up board))
    (:down (down board))))

(defun up (board-list)
  (transpose (left (transpose board-list))))

(defun down (board-list)
  (transpose (right (transpose board-list))))

(defun left (board-list)
  (loop for r from 0 to 3
       appending (left-row (row r board-list))))

(defun right (board-list)
  (loop for r from 0 to 3
     appending (right-row (row r board-list))))

(defun left-row (nums)
  (left-normalize-row (left-subrow (remove 0 nums))))

(defun left-normalize-row (nums)
  (let ((clean (remove 0 nums)))
    (append clean (loop repeat (- 4 (length clean)) collect 0))))

(defun left-subrow (nums)
  (cond
    ((null nums) nums)
    ((null (cdr nums)) nums)
    ((eql (first nums) (second nums))
     (cons (+ (first nums) (second nums))
	   (left-subrow (cddr nums))))
    (t (cons (first nums)
	     (left-subrow (cdr nums))))))

(defun right-row (nums)
  (reverse (left-row (reverse nums))))

(defun row (n l)
  "Returns row N of list L"
  (subseq l (* n 4) (* (1+ n) 4)))

(defun col (n l)
  (loop for r from 0 to 3 collect (nth (+ n (* r 4)) l)))

(defun transpose (l)
  (loop for r from 0 to 3 appending (col r l)))


(defun rows (board-list)
  (loop for r from 0 to 3 collecting (row r board-list)))
(defun columns (board-list)
  (rows (transpose board-list)))

(defun read-board (str)
  (let ((s1 (substitute-if
             #\Space
             (lambda (c) (member c '(#\+ #\- #\|)))
             (replace-all str
                          "|    "
                          "|   0"))))
    (read-from-string (format nil "(~A)" s1))))

(defun print-board (list)
  (loop
     for n from 1 to 4
     for row on list by #'cddddr
     do (format t "+----+----+----+----+~%")
       (format t (replace-all 
		  (format nil "|~4d|~4d|~4d|~4d|~%"
			  (first row)
			  (second row)
			  (third row)
			  (fourth row))
		  "|   0"
		  "|    ")))
  (format t "+----+----+----+----+~%")
  list)

;;; Taken from somewhere.
(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
			 :start2 old-pos
			 :test test)
       do (write-string string out
			:start old-pos
			:end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos))) 

;;; Placing a 2 or a 4

(defun place (number position board-list)
  (cond
    ((null board-list) nil)
    ((< number 0) (error 'error))
    ((eql position 0) (cons number (cdr board-list)))
    (t (cons (car board-list)
             (place number (1- position) (cdr board-list))))))

(defun all-cpu-moves (board-list)
  (mapcar-many (lambda (pos)
                 (list (place 2 pos board-list)
                       (place 4 pos board-list)))
               (positions 0 board-list)))

(defun %positions (index item sequence &key (key #'eql))
  (cond
    ((null sequence) nil)
    ((funcall key item (car sequence))
     (cons index (%positions (1+ index) item
                             (cdr sequence) :key key)))
    (t (%positions (1+ index) item
                   (cdr sequence) :key key))))

(defun positions (item sequence &key (key #'eql))
  (%positions 0 item sequence :key key))

(defun mapcar-many (function list)
  (apply #'append (mapcar function list)))

