
(defun parse-nanobot (line)
  (let* ((pos-start (+ (search "pos=<" line) (length "pos=<")))
         (pos-end (position #\> line :start pos-start))
         (r-start (+ (search "r=" line) (length "r=")))
         (pos-str (subseq line pos-start pos-end))
         (r-str (subseq line r-start))
         (coords (read-from-string (format nil "(~A)" (substitute #\Space #\, pos-str))))
         (r (parse-integer r-str)))
    (list coords r)))

(defun get-radius (nanobot) (second nanobot))
(defun get-position (nanobot) (first nanobot))

(defun manhattan-distance (p1 p2)
  (+ (abs (- (first p1) (first p2)))
     (abs (- (second p1) (second p2)))
     (abs (- (third p1) (third p2)))))

(defun main ()
  (let ((nanobots '()))
    (with-open-file (stream "input.txt" :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            do (push (parse-nanobot line) nanobots)))

    (let* ((strongest-nanobot (reduce (lambda (nb1 nb2)
                                        (if (> (get-radius nb1) (get-radius nb2))
                                            nb1
                                            nb2))
                                      nanobots))
           (strongest-pos (get-position strongest-nanobot))
           (strongest-r (get-radius strongest-nanobot))
           (count 0))
      (setf count (loop for nb in nanobots
                        count (<= (manhattan-distance (get-position nb) strongest-pos) strongest-r)))
      (format t "~A~%" count))))

(main)
