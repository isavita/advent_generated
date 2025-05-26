
(defun parse-range (line)
  (let ((pos (position #\- line)))
    (list (parse-integer line :end pos)
          (parse-integer line :start (1+ pos)))))

(defun compare-ranges (a b)
  (let ((start-a (first a))
        (start-b (first b)))
    (if (= start-a start-b)
        (< (second a) (second b))
        (< start-a start-b))))

(defun main ()
  (let (data)
    (with-open-file (f "input.txt" :direction :input)
      (loop for line = (read-line f nil)
            while line
            do (push (parse-range line) data)))

    (setf data (sort data #'compare-ranges))

    (let ((current-max 0)
          (allowed-ips 0)
          (total-max-ip 4294967295))

      (loop for (start end) in data
            do (when (> start current-max)
                 (incf allowed-ips (- start current-max 1)))
               (setf current-max (max current-max end)))

      (incf allowed-ips (- total-max-ip current-max))

      (format t "~A~%" allowed-ips))))

(main)
