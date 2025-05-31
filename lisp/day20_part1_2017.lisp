
(defun parse-coords (s)
  (loop for start = 0 then (1+ end)
        for end = (position #\, s :start start)
        collect (parse-integer (subseq s start (or end (length s))))
        while end))

(defun parse-line (line)
  (let* ((p-start (position #\< line :start (position #\p line)))
         (p-end (position #\> line :start p-start))
         (v-start (position #\< line :start (position #\v line)))
         (v-end (position #\> line :start v-start))
         (a-start (position #\< line :start (position #\a line)))
         (a-end (position #\> line :start a-start)))
    (list (parse-coords (subseq line (1+ p-start) p-end))
          (parse-coords (subseq line (1+ v-start) v-end))
          (parse-coords (subseq line (1+ a-start) a-end)))))

(defun manhattan (vec)
  (+ (abs (nth 0 vec))
     (abs (nth 1 vec))
     (abs (nth 2 vec))))

(defun main ()
  (let ((particles nil))
    (with-open-file (stream "input.txt" :direction :input)
      (loop for line = (read-line stream nil)
            while line
            do (push (parse-line line) particles)))
    (setf particles (nreverse particles))

    (let ((closest-particle-index 0)
          (min-accel most-positive-fixnum)
          (min-velocity most-positive-fixnum)
          (min-position most-positive-fixnum))

      (loop for particle in particles
            for i from 0
            do (let* ((p (nth 0 particle))
                      (v (nth 1 particle))
                      (a (nth 2 particle))
                      (accel (manhattan a))
                      (velocity (manhattan v))
                      (position (manhattan p)))
                 (when (or (< accel min-accel)
                           (and (= accel min-accel) (< velocity min-velocity))
                           (and (= accel min-accel) (= velocity min-velocity) (< position min-position)))
                   (setf min-accel accel
                         min-velocity velocity
                         min-position position
                         closest-particle-index i))))
      (format t "~a~%" closest-particle-index))))

(main)
