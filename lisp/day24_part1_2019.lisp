
(defun get-adjacent-bugs (g x y)
  (let ((c 0))
    (dolist (d '((-1 0) (1 0) (0 -1) (0 1)))
      (let* ((dx (car d))
             (dy (cadr d))
             (nx (+ x dx))
             (ny (+ y dy)))
        (when (and (>= nx 0) (< nx 5) (>= ny 0) (< ny 5)
                   (char= (nth nx (nth ny g)) #\#))
          (incf c))))
    c))

(defun update-grid (g)
  (loop for y from 0 to 4
        collect (loop for x from 0 to 4
                      collect (let* ((adj (get-adjacent-bugs g x y))
                                     (cur (nth x (nth y g))))
                                (cond
                                  ((and (char= cur #\#) (/= adj 1)) #\.)
                                  ((and (char= cur #\.) (or (= adj 1) (= adj 2))) #\#)
                                  (t cur))))))

(defun get-biodiversity-rating (g)
  (loop for y from 0 to 4
        sum (loop for x from 0 to 4
                  when (char= (nth x (nth y g)) #\#)
                    sum (expt 2 (+ (* y 5) x)))))

(defun main ()
  (let ((g nil)
        (s (make-hash-table)))
    (with-open-file (f "input.txt" :direction :input)
      (loop for l = (read-line f nil nil)
            while l
            do (push (coerce l 'list) g)))
    (setf g (nreverse g))

    (loop
      (let ((r (get-biodiversity-rating g)))
        (when (gethash r s)
          (print r)
          (return))
        (setf (gethash r s) t)
        (setf g (update-grid g))))))

(main)
