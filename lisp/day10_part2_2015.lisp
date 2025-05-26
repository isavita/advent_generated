
(defun look-and-say (s)
  (with-output-to-string (out)
    (loop with len = (length s)
          with i = 0
          while (< i len) do
      (let ((char (char s i))
            (count 0)
            (j i))
        (loop while (and (< j len) (char= (char s j) char)) do
          (incf count)
          (incf j))
        (format out "~a~a" count char)
        (setf i j)))))

(defun main ()
  (let ((sequence (with-open-file (file "input.txt" :direction :input)
                    (string-trim '(#\Newline #\Return) (read-line file)))))

    (loop for _ from 1 to 40 do
      (setf sequence (look-and-say sequence)))
    (format t "~a~%" (length sequence))

    (loop for _ from 1 to 10 do
      (setf sequence (look-and-say sequence)))
    (format t "~a~%" (length sequence))))

(main)
