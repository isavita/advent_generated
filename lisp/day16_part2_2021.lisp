
(defun hex-char-to-bin-str (char)
  (case char
    (#\0 "0000") (#\1 "0001") (#\2 "0010") (#\3 "0011")
    (#\4 "0100") (#\5 "0101") (#\6 "0110") (#\7 "0111")
    (#\8 "1000") (#\9 "1001") (#\A "1010") (#\B "1011")
    (#\C "1100") (#\D "1101") (#\E "1110") (#\F "1111")))

(defun hex-to-bin (hex-str)
  (with-output-to-string (s)
    (loop for char across hex-str
          do (write-string (hex-char-to-bin-str (char-upcase char)) s))))

(defun bin-str-to-int (s start end)
  (loop with result = 0
        for i from start below end
        do (setf result (logior (ash result 1)
                                (- (char-int (char s i)) (char-int #\0))))
        finally (return result)))

(defun calculate-result (type-id values)
  (case type-id
    (0 (apply #'+ values))
    (1 (apply #'* values))
    (2 (apply #'min values))
    (3 (apply #'max values))
    (5 (if (> (first values) (second values)) 1 0))
    (6 (if (< (first values) (second values)) 1 0))
    (7 (if (= (first values) (second values)) 1 0))))

(defun parse-packet (bin-str idx)
  (let* ((version (bin-str-to-int bin-str idx (+ idx 3)))
         (type-id (bin-str-to-int bin-str (+ idx 3) (+ idx 6)))
         (current-idx (+ idx 6)))
    (cond
      ((= type-id 4)
       (let ((value 0))
         (loop
           (let ((group-prefix (char bin-str current-idx)))
             (setf value (logior (ash value 4)
                                 (bin-str-to-int bin-str (1+ current-idx) (+ current-idx 5))))
             (incf current-idx 5)
             (when (char= group-prefix #\0)
               (return))))
         (values version current-idx value)))
      (t
       (let* ((length-type-id (bin-str-to-int bin-str current-idx (1+ current-idx)))
              (sub-packets-values '()))
         (incf current-idx 1)
         (if (= length-type-id 0)
             (let* ((total-length-of-sub-packets (bin-str-to-int bin-str current-idx (+ current-idx 15)))
                    (start-idx-of-sub-packets (+ current-idx 15)))
               (incf current-idx 15)
               (loop while (< (- current-idx start-idx-of-sub-packets) total-length-of-sub-packets)
                     do (multiple-value-bind (_ new-idx sub-value)
                            (parse-packet bin-str current-idx)
                          (push sub-value sub-packets-values)
                          (setf current-idx new-idx))))
             (let ((num-sub-packets (bin-str-to-int bin-str current-idx (+ current-idx 11))))
               (incf current-idx 11)
               (loop for i from 0 below num-sub-packets
                     do (multiple-value-bind (_ new-idx sub-value)
                            (parse-packet bin-str current-idx)
                          (push sub-value sub-packets-values)
                          (setf current-idx new-idx)))))
         (let ((result (calculate-result type-id (nreverse sub-packets-values))))
           (values version current-idx result)))))))

(defun main ()
  (with-open-file (in "input.txt" :direction :input)
    (let* ((hex-str (string-trim '(#\Space #\Tab #\Newline #\Return) (read-line in)))
           (bin-str (hex-to-bin hex-str)))
      (multiple-value-bind (version final-idx value)
          (parse-packet bin-str 0)
        (declare (ignore version final-idx))
        (format t "~a~%" value)))))

(main)
