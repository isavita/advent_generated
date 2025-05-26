
(defun get-angle (a b)
  (atan (- (second b) (second a)) (- (first b) (first a))))

(defun count-visible-asteroids (station asteroids)
  (let ((angles (make-hash-table :test 'equal)))
    (loop for asteroid in asteroids
          when (not (equal station asteroid))
          do (setf (gethash (get-angle station asteroid) angles) t))
    (hash-table-count angles)))

(defun find-best-location (asteroids)
  (loop with max-visible = 0
        for station in asteroids
        for visible = (count-visible-asteroids station asteroids)
        when (> visible max-visible)
        do (setf max-visible visible)
        finally (return max-visible)))

(defun main ()
  (let ((asteroids nil))
    (with-open-file (stream "input.txt" :direction :input)
      (loop for line = (read-line stream nil nil)
            for y from 0
            while line
            do (loop for char across line
                     for x from 0
                     when (char= char #\#)
                     do (push (list x y) asteroids))))
    (format t "~a~%" (find-best-location asteroids))))

(main)
