
(defstruct (reindeer
            (:constructor make-reindeer (speed fly-time rest-time)))
  speed
  fly-time
  rest-time)

(defun split-string (string &key (delimiter #\Space))
  (loop for i = 0 then (1+ j)
        for j = (position delimiter string :start i)
        collect (subseq string i (if j j (length string)))
        while j))

(defun read-reindeer-details (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect (let* ((parts (split-string line))
                         (speed (parse-integer (nth 3 parts)))
                         (fly-time (parse-integer (nth 6 parts)))
                         (rest-time (parse-integer (nth 13 parts))))
                    (make-reindeer speed fly-time rest-time)))))

(defun calculate-distance (reindeer total-seconds)
  (let* ((speed (reindeer-speed reindeer))
         (fly-time (reindeer-fly-time reindeer))
         (rest-time (reindeer-rest-time reindeer))
         (cycle-time (+ fly-time rest-time))
         (full-cycles (floor total-seconds cycle-time))
         (remaining-seconds (mod total-seconds cycle-time)))
    (+ (* full-cycles speed fly-time)
       (* speed (min fly-time remaining-seconds)))))

(defun main ()
  (let* ((reindeers (read-reindeer-details "input.txt"))
         (total-seconds 2503))
    (format t "~a~%" (loop for r in reindeers
                          maximize (calculate-distance r total-seconds)))))

(main)
