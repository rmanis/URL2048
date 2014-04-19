;;; Heuristics

(in-package :twenty-forty-eight)

(defun fitness (board-list)
  (let* ((zeroes (count 0 board-list))
         ;;(adjacents (count-adjacents board-list))
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

(defun minimax (board-list &key (search-depth 3) (heuristic #'fitness))
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