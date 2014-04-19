
(in-package :twenty-forty-eight)

(defun html-board (board-url-string)
  (let* ((tokens (split-sequence:split-sequence #\+ board-url-string))
         (count (length tokens)))
    (cond
      ((< count 18) "invalid url")
      (t (mapcar #'read-from-string tokens)))
    ))

(defun construct-html (game-state)
  (let* ((board (subseq game-state 0 15))
         (player (nth 16 board))
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
  <head></head>
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
    (loop for index from (* row-number 4)
          for num in (row row-number board)
          do
       (format s "<td>~A</td>" (make-tile-link board num index player)))
    (format s "</tr>")))

(defun make-tile-link (board number index player)
  (cond
    ((and (eq player :cpu)
          (eql number 0))
     (format nil "~A ~A"
             (make-link "2" (place 2 index board)
                        :player :human
                        :link-class "cpu-move")
             (make-link "4" (place 4 index board)
                        :player :human
                        :link-class "cpu-move")))
    (t (format nil "~D" number))))

(defun make-sliders (board player)
  (when (eq player 'human)
    (let ((futures (determine-slides board)))
      (format nil "<table>
           <tr><td>&nbsp;</td><td>~A</td><td>&nbsp;</td></tr>
           <tr><td>~A</td><td>&nbsp;</td><td>~A</td></tr>
           <tr><td>&nbsp;</td><td>~A</td><td>&nbsp;</td></tr>
           </table>"
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

(defun encode-board (board player)
  (let* ((stripped 
          (cl-ppcre:regex-replace-all
           "\\(|\\)" (format nil "~S" board) ""))
         (conjoined (cl-ppcre:regex-replace-all " " stripped "+")))
    (format nil "~A+~A" conjoined player)))
           
