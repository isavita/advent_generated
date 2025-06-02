
(defun read-signal (filename)
  (with-open-file (stream filename :direction :input)
    (let ((line (read-line stream nil "")))
      (map 'vector (lambda (char) (- (char-code char) (char-code #\0))) line))))

(defconstant +base-pattern+ '#(0 1 0 -1))

(defun fft (signal)
  (let* ((len (length signal))
         (output (make-array len :element-type 'fixnum)))

    (let ((sum 0))
      (loop for j from 0 below len
            do (incf sum (* (aref signal j)
                            (aref +base-pattern+ (mod (floor (/ (1+ j) 1)) 4)))))
      (setf (aref output 0) (mod (abs sum) 10)))

    (loop for i from 1 below (floor len 2)
          do (let ((sum 0))
               (loop for j from i below len
                     do (incf sum (* (aref signal j)
                                     (aref +base-pattern+ (mod (floor (/ (1+ j) (1+ i))) 4)))))
               (setf (aref output i) (mod (abs sum) 10))))

    (setf (aref output (1- len)) (mod (aref signal (1- len)) 10))
    (loop for i from (- len 2) downto (floor len 2)
          do (setf (aref output i)
                   (mod (+ (aref signal i) (aref output (1+ i))) 10)))

    output))

(defun main ()
  (let ((signal (read-signal "input.txt")))
    (loop repeat 100
          do (setf signal (fft signal)))
    (loop for i from 0 below 8
          do (princ (aref signal i)))))

(main)
