
(defconstant +dir-n+ 0)
(defconstant +dir-e+ 1)
(defconstant +dir-s+ 2)
(defconstant +dir-w+ 3)

(defstruct (p (:print-object (lambda (obj stream) (format stream "#<P ~a ~a>" (p-x obj) (p-y obj)))))
  x y)

(defstruct movement
  steps
  rotate)

(defstruct human
  curr
  facing)

(defparameter *dirs* nil)

(defun rotate-dir (dir char)
  (cond ((char= char #\R) (mod (+ dir 1) 4))
        ((char= char #\L) (mod (+ dir -1 4) 4))
        (t dir)))

(defun dir-points (dir)
  (mod (+ dir 3) 4))

(defun cross-border (pos dir size)
  (let ((x (p-x pos))
        (y (p-y pos)))
    (cond
      ((and (= x -1) (< y (* 2 size))) (values (make-p :x (+ y (* 2 size)) :y (+ x 1)) +dir-e+))
      ((and (= x -1) (>= y (* 2 size))) (values (make-p :x (+ x (* 4 size)) :y (- y (* 2 size))) +dir-n+))
      ((and (= x size) (= dir +dir-s+)) (values (make-p :x (- y size) :y (+ x size -1)) +dir-w+))
      ((and (= x (- (* 2 size) 1)) (= dir +dir-n+)) (values (make-p :x (+ y size) :y (+ x (- size) 1)) +dir-e+))
      ((and (= x (* 3 size)) (= dir +dir-s+)) (values (make-p :x (+ y (* 2 size)) :y (- x (* 2 size) 1)) +dir-w+))
      ((= x (* 4 size)) (values (make-p :x (- x (* 4 size)) :y (+ y (* 2 size))) +dir-s+))
      ((and (= y -1) (< x (* 3 size))) (values (make-p :x (- (* 3 size) 1 x) :y (+ y size 1)) +dir-e+))
      ((and (= y -1) (>= x (* 3 size))) (values (make-p :x (+ y 1) :y (- x (* 2 size))) +dir-s+))
      ((and (= y (- size 1)) (< x size)) (values (make-p :x (- (* 3 size) 1 x) :y (+ y (- size) 1)) +dir-e+))
      ((and (= y (- size 1)) (>= x size) (= dir +dir-w+)) (values (make-p :x (+ y size 1) :y (- x size)) +dir-s+))
      ((and (= y size) (= dir +dir-e+)) (values (make-p :x (+ y (* 2 size) -1) :y (- x (* 2 size))) +dir-n+))
      ((and (= y (* 2 size)) (< x (* 2 size)) (= dir +dir-e+)) (values (make-p :x (- y size 1) :y (+ x size)) +dir-n+))
      ((and (= y (* 2 size)) (>= x (* 2 size))) (values (make-p :x (- (* 3 size) 1 x) :y (+ y size -1)) +dir-w+))
      ((= y (* 3 size)) (values (make-p :x (- (* 3 size) 1 x) :y (- y size 1)) +dir-w+))
      (t (error "Not a border crossing")))))

(defun human-walk (human map-data dirs size)
  (let* ((curr (human-curr human))
         (facing (human-facing human))
         (dir-delta (elt dirs facing))
         (next-pos (make-p :x (+ (p-x curr) (p-x dir-delta))
                           :y (+ (p-y curr) (p-y dir-delta)))))
    (multiple-value-bind (map-val exists)
        (gethash next-pos map-data)
      (if exists
          (if map-val
              (values curr facing)
              (values next-pos facing))
          (multiple-value-bind (new-pos new-facing)
              (cross-border next-pos facing size)
            (multiple-value-bind (new-map-val new-exists)
                (gethash new-pos map-data)
              (if (and new-exists new-map-val)
                  (values curr facing)
                  (values new-pos new-facing))))))))

(defun parse-path (path-str)
  (let ((movements nil)
        (acc 0))
    (loop for char across path-str do
      (cond
        ((or (char= char #\R) (char= char #\L))
         (when (/= acc 0)
           (push (make-movement :steps acc) movements)
           (setf acc 0))
         (push (make-movement :rotate (string char)) movements))
        ((digit-char-p char)
         (setf acc (+ (* acc 10) (digit-char-p char))))))
    (when (/= acc 0)
      (push (make-movement :steps acc) movements))
    (nreverse movements)))

(defun read-lines (stream)
  (loop for line = (read-line stream nil nil)
        while line
        collect line))

(defun parse-input (filename)
  (let ((map-data (make-hash-table :test #'equalp))
        (size 0)
        (lines nil))
    (with-open-file (f filename :direction :input)
      (setf lines (read-lines f)))
    (let ((r 0)
          (map-parsed-end-idx 0))
      (loop for line in lines
            while (not (string= (string-trim '(#\Newline #\Return) line) ""))
            do
               (setf line (string-trim '(#\Newline #\Return) line))
               (when (= r 0)
                 (setf size (floor (length line) 3)))
               (loop for c from 0 below (length line)
                     for char = (char line c)
                     do
                        (cond
                          ((char= char #\#) (setf (gethash (make-p :x r :y c) map-data) t))
                          ((char= char #\.) (setf (gethash (make-p :x r :y c) map-data) nil))))
               (incf r)
               (setf map-parsed-end-idx r))
      (let* ((movement-line (nth (+ map-parsed-end-idx 1) lines))
             (movements (parse-path (string-trim '(#\Newline #\Return) movement-line))))
        (values map-data size movements)))))

(defun main ()
  (setf *dirs* (list (make-p :x -1 :y 0)
                     (make-p :x 0 :y 1)
                     (make-p :x 1 :y 0)
                     (make-p :x 0 :y -1)))
  (multiple-value-bind (map-data size movements) (parse-input "input.txt")
    (let ((human (make-human :curr (make-p :x 0 :y size) :facing +dir-e+)))
      (loop for mov in movements do
        (if (movement-rotate mov)
            (setf (human-facing human) (rotate-dir (human-facing human) (char (movement-rotate mov) 0)))
            (dotimes (i (movement-steps mov))
              (multiple-value-bind (new-pos new-facing)
                  (human-walk human map-data *dirs* size)
                (when (and (equalp new-pos (human-curr human))
                           (= new-facing (human-facing human)))
                  (return))
                (setf (human-curr human) new-pos)
                (setf (human-facing human) new-facing)))))
      (let ((final-value (+ (* 1000 (+ (p-x (human-curr human)) 1))
                            (* 4 (+ (p-y (human-curr human)) 1))
                            (dir-points (human-facing human)))))
        (format t "~a~%" final-value)))))

(main)
