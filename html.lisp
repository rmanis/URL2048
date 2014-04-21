
(in-package :twenty-forty-eight)

(defvar *url-style* :letters)

(defun determine-style (board-string)
  (let ((char (char-code (aref board-string 0))))
    (cond
      ((and (<= char (char-code #\9))
            (>= char (char-code #\0)))
       :spaces)
      ((and (<= char (char-code #\Z))
            (>= char (char-code #\A)))
       :letters))))

(defun board-powers (board-list)
  (mapcar (lambda (x) (floor (if (> x 0) (log x 2) 0)))
          board-list))

(defun html-board-letters (board-url-string)
  (if (not (eql (length board-url-string) 17))
      "invalid url"
      (let* ((board (decode-tiles (subseq board-url-string 0 16)))
             (player (decode-player (aref board-url-string 16))))
        (construct-html (append board (list player))))))

(defun html-board-spaces (board-url-string)
  (let* ((tokens (split-sequence:split-sequence #\Space board-url-string))
         (count (length tokens)))
    (cond
      ((< count 17) "invalid url")
      (t (construct-html (mapcar #'read-from-string tokens))))
    ))

(defun construct-html (game-state)
  (let* ((board (subseq game-state 0 16))
         (player (nth 16 game-state))
         (header (make-header))
         (footer (make-footer))
         (html-board (make-html-board board player))
         (sliders (make-sliders board player)))
    (concatenate 'string
                 header
                 html-board
                 sliders
                 footer)))

(defun make-header ()
  "
<html>
  <head>
    <link href=\"style.css\" rel=\"stylesheet\" type=\"text/css\" />
  </head>
  <body>
")

(defun make-footer ()
  "</body></html>")

(defun make-html-board (board player)
  (concatenate
   'string "<table>"
   (make-html-row 0 board player)
   (make-html-row 1 board player)
   (make-html-row 2 board player)
   (make-html-row 3 board player)
   "</table>"))

(defun make-html-row (row-number board player)
  (with-output-to-string (s)
    (format s "<tr>")
    (loop with start-index = (* row-number 4)
          for index from start-index to (+ start-index 3)
          for num = (nth index board)
          do
       (format s "<td>~A</td>" (make-tile-link board num index player)))
    (format s "</tr>")))

(defun make-tile-link (board number index player)
  (cond
    ((and (eq player 'cpu)
          (eql number 0))
     (format nil "~A ~A"
             (make-link "2" (place 2 index board)
                        :player 'human
                        :link-class "cpu-move")
             (make-link "4" (place 4 index board)
                        :player 'human
                        :link-class "cpu-move")))
    ((not (eql number 0)) (format nil "~D" number))
    (t "")))

(defun make-sliders (board player)
  (when (eq player 'human)
    (let ((futures (determine-slides board)))
      (format nil "<table>
           <tr><td>&nbsp;</td><td>~A</td><td>&nbsp;</td></tr>
           <tr><td>~A</td><td>&nbsp;</td><td>~A</td></tr>
           <tr><td>&nbsp;</td><td>~A</td><td>&nbsp;</td></tr>
           </table>~%"
              (make-slide-link :up futures)
              (make-slide-link :left futures)
              (make-slide-link :right futures)
              (make-slide-link :down futures)))))

(defun make-slide-link (direction futures)
  (let ((result (cadr (find direction futures
                            :key #'car))))
    (if result
        (make-link (symbol-name direction) result :player :cpu)
        (symbol-name direction))))

(defun make-link (text future &key (player :cpu) link-class)
  (format nil "<a ~Ahref=\"2048.cgi?board=~A\">~A</a>"
          (if link-class (format nil "class=\"~A\" " link-class) "")
          (encode-board future player)
          text))

(defun encode-tile (number)
  (if (eql number 0)
      #\A
      (code-char
       (+ (char-code #\A)
          (floor (log number 2))))))

(defun decode-tile (letter)
  (if (eql letter #\A)
      0
      (expt 2 (- (char-code letter) (char-code #\A)))))

(defun decode-tiles (string)
  (map 'list #'decode-tile string))

(defun decode-player (char)
  (case char
    (#\A 'cpu)
    (#\B 'human)))

(defun encode-board (board player)
  (case *url-style*
    (:spaces
       (let* ((stripped
               (cl-ppcre:regex-replace-all
                "\\(|\\)" (format nil "~S" board) ""))
              (conjoined (cl-ppcre:regex-replace-all " " stripped "+")))
         (format nil "~A+~A" conjoined player)))
    (:letters
       (with-output-to-string (s)
         (format s "~A~A"
                 (map 'string #'encode-tile board)
                 (if (eq player 'human) "B" "A"))))))
