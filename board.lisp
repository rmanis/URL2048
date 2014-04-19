
(require :trivial-http)

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

(defun fitness (board-list)
  (let* ((zeroes (count 0 board-list))
         (adjacents (count-adjacents board-list))
         (weighted-adjacents (count-adjacents-weighted board-list))
         (biggest (apply #'max board-list))
         (power (floor (log biggest 2))))
    (+ zeroes
       #+nil adjacents
       weighted-adjacents
       (* 2 power))))

(defun count-adjacents (board-list)
  (let ((row-adjacents
	 (loop for row in (rows board-list)
               summing (count-row-adjacents (remove 0 row))))
	(column-adjacents
	 (loop for column in (columns board-list)
               summing (count-row-adjacents (remove 0 column)))))
    (+ row-adjacents column-adjacents)))

(defun count-adjacents-weighted (board-list)
  (let ((row-adjacents-weighted
         (loop for row in (rows board-list)
               summing (count-row-adjacents-weighted (remove 0 row))))
        (column-adjacents-weighted
         (loop for column in (columns board-list)
               summing (count-row-adjacents-weighted (remove 0 column)))))
    (+ row-adjacents-weighted column-adjacents-weighted)))

(defun count-row-adjacents-weighted (nums &optional (acc 0))
  (let ((first (first nums)))
    (if nums
        (count-row-adjacents
         (cdr nums)
         (if (eql first (second nums))
             (+ acc (floor (log first 2)))
             acc))
        acc)))

(defun count-row-adjacents (nums &optional (acc 0))
  (if nums
      (count-row-adjacents (cdr nums)
			   (if (eql (first nums) (second nums))
			       (1+ acc)
			       acc))
      acc))

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

;;; Playing on paste.lisp.org

(defun slurp-http (url)
  (let ((result (trivial-http:http-get url)))
    (destructuring-bind (status headers stream) result
      (declare (ignore status))
      (let ((seq (make-string (read-from-string (cdr (assoc :content-length headers))))))
	(read-sequence seq stream)
	seq))))

(defun print-all-moves (url)
  (let* ((board (slurp-http url))
	 (directions '(:up :right :left :down))
	 (boards (mapcar (lambda (x) (move-board x (read-board board)))
			 directions)))
    (princ board) (terpri)
    (loop
       for direction in directions
       for possibility in boards
       do
	 (princ direction)
	 (terpri)
	 (print-board possibility)
	 (terpri))))

(defun move-http (url direction)
  (move direction (slurp-http url)))

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

;;; Minimax

(defun determine-slides (board-list)
  "Calculate the ways to slide the numbers on the board.  Returns a
list of moves, where each move is a list containing
 - the direction of the move
 - the board after that move is made"
  (remove-if (lambda (x) (equal (second x) board-list))
             (mapcar (lambda (direction)
                       (list direction (move-board direction board-list)))
                     '(:up :right :down :left))))

(defun higher-future (future-a future-b)
  (let ((a-score (car future-a))
        (b-score (car future-b)))
    (when (and a-score b-score)
      (if (>= a-score b-score)
          future-a
          future-b))))

(defun minimax (board-list &key (search-depth 4) (heuristic #'fitness))
  "Minimax function that determines the slider's optimal move.
Returns a list containing
 - The heuristic
 - The path to get there"
  (if (<= search-depth 0)
      (list (funcall heuristic board-list) board-list)
      (let* ((moves (determine-slides board-list))
             (evaluations
              (mapcar
               (lambda (future) ; (:up (0 2 4 ...)
                 (let ((them (maximin (second future)
                                      :search-depth (1- search-depth)
                                      :heuristic heuristic)))
                   ; them: (SCORE HISTORY)
                   (list (first them)
                         (first future)
                         (second future))))
               moves)))
         (if evaluations
             (let ((answer (reduce #'higher-future evaluations)))
               (values answer (list 'quote (third answer))))
             (list 0 :dead)))))

(defun lowest-future (futures)
  (reduce
   (lambda (a b)
     (let ((score-a (first a))
           (score-b (first b)))
       (if (<= score-a score-b)
           a
           b)))
   futures))

(defun maximin (board-list &key (search-depth 3) (heuristic #'fitness))
  "Minimax function that determines the number-placer's optimal move.
That is, the future with the lowest score returned by the HEURISTIC
function.  Returns a list containing the minimum score and the path to
it."
  (if (<= search-depth 0)
      (list (funcall heuristic board-list) nil)
      (let* ((possibilities (all-cpu-moves board-list))
             (results                   ; ex ((9 :left) (10 :up))
              (mapcar (lambda (board)
                        (minimax board
                                 :search-depth search-depth
                                 :heuristic heuristic))
                      possibilities)))
        (when results
          (lowest-future results)))))