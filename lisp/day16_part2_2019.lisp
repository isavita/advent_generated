
(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defun repeat-input (input-str times)
  (declare (type string input-str)
           (type fixnum times))
  (let* ((input-len (length input-str))
         (total-len (* input-len times))
         (digits (make-array total-len :element-type 'fixnum)))
    (declare (type fixnum input-len total-len)
             (type (simple-array fixnum (*)) digits))
    (loop for t-idx of-type fixnum from 0 below times
          do (loop for i of-type fixnum from 0 below input-len
                   for char of-type character = (char input-str i)
                   for digit of-type fixnum = (- (char-code char) (char-code #\0))
                   for target-idx of-type fixnum = (+ (* t-idx input-len) i)
                   do (setf (aref digits target-idx) digit)))
    digits))

(defun main ()
  (let* ((input-str (with-open-file (f "input.txt" :direction :input :if-does-not-exist :error)
                       (read-line f)))
         (offset (parse-integer (subseq input-str 0 7)))
         (repeated-input (repeat-input input-str 10000)))
    (declare (type string input-str)
             (type fixnum offset)
             (type (simple-array fixnum (*)) repeated-input))

    (loop for phase of-type fixnum from 0 below 100
          do (loop with sum of-type fixnum = 0
                   for i of-type fixnum from (1- (length repeated-input)) downto offset
                   do (incf sum (aref repeated-input i))
                      (setf (aref repeated-input i) (mod sum 10))))

    (loop for i of-type fixnum from offset below (+ offset 8)
          collect (aref repeated-input i) into result-digits
          finally (format t "~{~a~}~%" result-digits)))
  #+sbcl (sb-ext:exit :code 0))

(main)
