
(defstruct board
  (numbers (make-array '(5 5) :initial-element 0))
  (marked (make-array '(5 5) :initial-element nil)))

(defun board-mark (board number)
  (loop for i from 0 below 5
        do (loop for j from 0 below 5
                 when (= (aref (board-numbers board) i j) number)
                 do (setf (aref (board-marked board) i j) t))))

(defun is-row-marked (board row-idx)
  (loop for j from 0 below 5
        always (aref (board-marked board) row-idx j)))

(defun is-column-marked (board col-idx)
  (loop for i from 0 below 5
        always (aref (board-marked board) i col-idx)))

(defun board-has-won (board)
  (loop for i from 0 below 5
        thereis (or (is-row-marked board i)
                    (is-column-marked board i))))

(defun board-unmarked-sum (board)
  (loop for i from 0 below 5
        sum (loop for j from 0 below 5
                  when (not (aref (board-marked board) i j))
                  sum (aref (board-numbers board) i j))))

(defun split-string (string delimiter)
  (loop for i = 0 then (1+ j)
        for j = (position delimiter string :start i)
        collect (subseq string i j)
        while j))

(defun parse-space-separated-numbers (line)
  (with-input-from-string (s line)
    (loop for num = (read s nil :eof)
          until (eq num :eof)
          collect num)))

(defun main ()
  (let* ((lines (with-open-file (s "input.txt")
                  (loop for line = (read-line s nil nil)
                        while line
                        collect line)))
         (numbers (mapcar #'parse-integer (split-string (first lines) #\,)))
         (boards (loop for i from 2 below (length lines) by 6
                       collect (let ((b (make-board)))
                                 (loop for j from 0 below 5
                                       for row-numbers = (parse-space-separated-numbers (nth (+ i j) lines))
                                       do (loop for col from 0 below 5
                                                do (setf (aref (board-numbers b) j col) (nth col row-numbers))))
                                 b)))
         (winning-board nil)
         (winning-number 0))

    (loop for num in numbers
          do (loop for board in boards
                   do (board-mark board num)
                      (when (board-has-won board)
                        (setf winning-board board)
                        (setf winning-number num)
                        (return)))
          when winning-board
          do (return))

    (when winning-board
      (print (* (board-unmarked-sum winning-board) winning-number)))))

(main)
