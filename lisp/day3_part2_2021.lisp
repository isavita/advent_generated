
(defun filter-by-bit (values bit-index keep-char)
  (remove-if-not (lambda (val)
                   (char= (char val bit-index) keep-char))
                 values))

(defun filter-values (initial-values criteria)
  (let* ((num-bits (length (first initial-values))))
    (loop with current-values = initial-values
          for i from 0 below num-bits
          do (when (= (length current-values) 1)
               (return (first current-values)))
             (let* ((zeros 0)
                    (ones 0))
               (loop for val in current-values
                     do (if (char= (char val i) #\0)
                            (incf zeros)
                            (incf ones)))
               (let ((keep-char (funcall criteria zeros ones)))
                 (setf current-values (filter-by-bit current-values i keep-char))))
          finally (return (first current-values)))))

(defun read-input (filename)
  (with-open-file (in filename :direction :input)
    (loop for line = (read-line in nil nil)
          while line
          collect line)))

(defun main ()
  (let* ((values (read-input "input.txt"))
         (oxygen-generator-rating-str
           (filter-values values
                          (lambda (zeros ones)
                            (if (> zeros ones) #\0 #\1))))
         (oxygen-generator-rating-int
           (parse-integer oxygen-generator-rating-str :radix 2))
         (co2-scrubber-rating-str
           (filter-values values
                          (lambda (zeros ones)
                            (if (<= zeros ones) #\0 #\1))))
         (co2-scrubber-rating-int
           (parse-integer co2-scrubber-rating-str :radix 2)))
    (format t "~a~%" (* oxygen-generator-rating-int co2-scrubber-rating-int))))

(main)
