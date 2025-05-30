
(defstruct mirror
  rows
  cols)

(defun parse-mirror (mirror-str)
  (let* ((num-rows (length mirror-str))
         (num-cols (length (car mirror-str)))
         (rows (make-array num-rows :initial-element 0))
         (cols (make-array num-cols :initial-element 0)))
    (loop for y from 0 below num-rows
          for row-line = (elt mirror-str y) do
      (loop for x from 0 below num-cols do
        (setf (aref rows y) (ash (aref rows y) 1))
        (setf (aref cols x) (ash (aref cols x) 1))
        (when (char= (char row-line x) #\#)
          (incf (aref rows y))
          (incf (aref cols x)))))
    (make-mirror :rows rows :cols cols)))

(defun parse-input (lines)
  (let ((mirrors (list))
        (current-mirror-lines (list)))
    (loop for line in lines do
      (if (string= line "")
          (progn
            (unless (null current-mirror-lines)
              (push (parse-mirror (nreverse current-mirror-lines)) mirrors))
            (setf current-mirror-lines (list)))
          (push line current-mirror-lines)))
    (unless (null current-mirror-lines)
      (push (parse-mirror (nreverse current-mirror-lines)) mirrors))
    (nreverse mirrors)))

(defun get-mirror-axis (lines)
  (let ((len (length lines)))
    (loop for i from 1 below len do
      (let ((is-mirror t)
            (limit (min i (- len i))))
        (loop for j from 0 below limit
              while is-mirror do
          (unless (= (aref lines (- i 1 j)) (aref lines (+ i j)))
            (setf is-mirror nil)))
        (when is-mirror
          (return-from get-mirror-axis i))))
    0))

(defun get-mirror-axis-with-one-smudge (lines)
  (let ((len (length lines)))
    (loop for i from 1 below len do
      (let ((num-smudges 0)
            (is-mirror t)
            (limit (min i (- len i))))
        (loop for j from 0 below limit
              when (not (= (aref lines (- i 1 j)) (aref lines (+ i j)))) do
          (let* ((v1 (aref lines (- i 1 j)))
                 (v2 (aref lines (+ i j)))
                 (dif (logxor v1 v2)))
            (if (zerop (logand dif (1- dif)))
                (incf num-smudges)
                (setf is-mirror nil)))
          when (> num-smudges 1) do
            (setf is-mirror nil)
          while is-mirror)
        (when (and is-mirror (= num-smudges 1))
          (return-from get-mirror-axis-with-one-smudge i))))
    0))

(defun solve (input-lines)
  (let ((mirrors (parse-input input-lines))
        (total-res 0))
    (loop for mirror in mirrors do
      (incf total-res (get-mirror-axis (mirror-cols mirror)))
      (incf total-res (* (get-mirror-axis (mirror-rows mirror)) 100)))
    total-res))

(defun read-file-lines (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil :eof)
          until (eq line :eof)
          collect line)))

(defun main ()
  (let* ((input-lines (read-file-lines "input.txt"))
         (result (solve input-lines)))
    (format t "~a~%" result)))

(main)
