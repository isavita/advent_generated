
(defun move-tail (head tail)
  (destructuring-bind (hx hy) head
    (destructuring-bind (tx ty) tail
      (let ((diff-x (- hx tx))
            (diff-y (- hy ty)))
        (if (and (<= (abs diff-x) 1) (<= (abs diff-y) 1))
            tail
            (list (+ tx (signum diff-x))
                  (+ ty (signum diff-y))))))))

(defun split-string (string delimiter)
  (loop for start = 0 then (1+ end)
        for end = (position delimiter string :start start)
        collect (subseq string start (or end (length string)))
        while end))

(defun simulate-rope (motions)
  (let ((knots (loop repeat 10 collect (list 0 0)))
        (visited (make-hash-table :test 'equal)))
    (let ((initial-tail (elt knots 9)))
      (setf (gethash (format nil "~a,~a" (car initial-tail) (cadr initial-tail)) visited) t))

    (loop for motion in motions do
      (let* ((parts (split-string motion #\Space))
             (direction (char (car parts) 0))
             (step-count (parse-integer (cadr parts))))
        (loop for step from 1 to step-count do
          (let ((head-pos (car knots)))
            (case direction
              (#\R (incf (car head-pos)))
              (#\L (decf (car head-pos)))
              (#\U (incf (cadr head-pos)))
              (#\D (decf (cadr head-pos)))))

          (loop for i from 1 below (length knots) do
            (setf (nth i knots) (move-tail (nth (1- i) knots) (nth i knots))))

          (let ((current-tail (elt knots 9)))
            (setf (gethash (format nil "~a,~a" (car current-tail) (cadr current-tail)) visited) t)))))
    (hash-table-count visited)))

(defun main ()
  (let* ((input-file "input.txt")
         (motions (with-open-file (stream input-file :direction :input)
                    (loop for line = (read-line stream nil)
                          while line
                          collect (string-trim '(#\Space #\Tab #\Newline #\Return) line)))))
    (format t "~a~%" (simulate-rope motions))))

(main)
