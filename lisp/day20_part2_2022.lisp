
(defun read-input ()
  (with-open-file (f "input.txt" :direction :input)
    (loop for line = (read-line f nil)
          while line
          collect (parse-integer line))))

(defun mix (nums)
  (let* ((L (length nums))
         (n (1- L)))
    (loop for i from 0 below L
          do (let* ((current-element (svref nums i))
                    (oldpos (car current-element))
                    (val (cdr current-element))
                    (newpos (mod (+ oldpos val) n)))
               (when (< oldpos newpos)
                 (loop for j from 0 below L
                       for other-element = (svref nums j)
                       for other-pos = (car other-element)
                       when (and (> other-pos oldpos) (<= other-pos newpos))
                       do (setf (car other-element) (1- other-pos))))
               (when (< newpos oldpos)
                 (loop for j from 0 below L
                       for other-element = (svref nums j)
                       for other-pos = (car other-element)
                       when (and (>= other-pos newpos) (< other-pos oldpos))
                       do (setf (car other-element) (1+ other-pos))))
               (setf (car current-element) newpos)))))

(defun coords (nums)
  (let* ((L (length nums))
         (zero-pos 0))
    (loop for i from 0 below L
          for element = (svref nums i)
          when (zerop (cdr element))
          do (setf zero-pos (car element))
             (return))
    (loop for i from 0 below L
          for element = (svref nums i)
          for current-pos = (car element)
          for value = (cdr element)
          sum (if (or (= current-pos (mod (+ zero-pos 1000) L))
                      (= current-pos (mod (+ zero-pos 2000) L))
                      (= current-pos (mod (+ zero-pos 3000) L)))
                  value
                  0))))

(defun main ()
  (let* ((input-values (read-input))
         (L (length input-values))
         (nums2 (make-array L :initial-element nil)))
    (loop for i from 0 below L
          for val in input-values
          do (setf (svref nums2 i) (cons i (* 811589153 val))))
    (loop repeat 10
          do (mix nums2))
    (print (coords nums2))))

(main)
