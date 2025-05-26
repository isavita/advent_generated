
(ql:quickload :alexandria)

(defun calculate-happiness-change (seating happiness-map)
  (let ((total-change 0)
        (n (length seating)))
    (loop for i from 0 below n
          for person = (elt seating i)
          for left-neighbor = (elt seating (mod (1- i) n))
          for right-neighbor = (elt seating (mod (1+ i) n))
          do
             (incf total-change (gethash left-neighbor (gethash person happiness-map)))
             (incf total-change (gethash right-neighbor (gethash person happiness-map))))
    total-change))

(defun main ()
  (let ((happiness (make-hash-table :test 'equal))
        (people-set (make-hash-table :test 'equal))
        (optimal-happiness most-negative-double-float))
    (with-open-file (file "input.txt" :direction :input)
      (loop for line = (read-line file nil nil)
            while line
            do
               (let* ((parts (alexandria:split-sequence #\Space line))
                      (person (car parts))
                      (neighbor (string-right-trim "." (car (last parts))))
                      (happiness-change (parse-integer (nth 3 parts))))
                 (when (string= (nth 2 parts) "lose")
                   (setf happiness-change (- happiness-change)))
                 (setf (gethash neighbor (alexandria:ensure-gethash person happiness (lambda () (make-hash-table :test 'equal)))) happiness-change)
                 (setf (gethash person people-set) t))))
    (let ((people-list (alexandria:hash-table-keys people-set)))
      (loop for seating-list in (alexandria:permutations people-list)
            for seating-vector = (coerce seating-list 'vector)
            for current-happiness = (calculate-happiness-change seating-vector happiness)
            do
               (setf optimal-happiness (max optimal-happiness current-happiness))))
    (format t "~a~%" optimal-happiness)))

(main)
