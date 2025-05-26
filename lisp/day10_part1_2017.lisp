
(defun parse-lengths (s)
  (loop with current-pos = 0
        with len = (length s)
        for next-comma = (position #\, s :start current-pos)
        for end-pos = (if next-comma next-comma len)
        collect (parse-integer s :start current-pos :end end-pos)
        do (setf current-pos (if next-comma (1+ next-comma) nil))
        while current-pos))

(defun main ()
  (let* ((lengths (with-open-file (f "input.txt" :direction :input)
                     (parse-lengths (read-line f))))
         (knot-list (make-array 256 :element-type 'fixnum
                                    :initial-contents (loop for i from 0 to 255 collect i)))
         (current-position 0)
         (skip-size 0))
    (declare (type (vector fixnum 256) knot-list)
             (type fixnum current-position skip-size))

    (dolist (length lengths)
      (dotimes (i (floor length 2))
        (let* ((start (mod (+ current-position i) 256))
               (end (mod (- (+ current-position length -1) i) 256)))
          (rotatef (aref knot-list start) (aref knot-list end))))
      (setf current-position (mod (+ current-position length skip-size) 256))
      (incf skip-size))

    (print (* (aref knot-list 0) (aref knot-list 1)))))

(main)
