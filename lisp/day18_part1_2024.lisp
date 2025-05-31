
(defconstant +size+ 71)

(defvar *grid* (make-array (list +size+ +size+) :initial-element nil))
(defvar *visited* (make-array (list +size+ +size+) :initial-element nil))
(defvar *dirs* '((1 0) (-1 0) (0 1) (0 -1)))

(defun split-string (string delimiter)
  (loop for start = 0 then (1+ end)
        for end = (position delimiter string :start start)
        collect (subseq string start (or end (length string)))
        while end))

(defvar *q-in* nil)
(defvar *q-out* nil)

(defun enqueue (item)
  (push item *q-in*))

(defun dequeue ()
  (unless *q-out*
    (when *q-in*
      (setf *q-out* (nreverse *q-in*))
      (setf *q-in* nil)))
  (when *q-out*
    (pop *q-out*)))

(defun queue-empty-p ()
  (and (null *q-in*) (null *q-out*)))

(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (loop for i from 0 below 1024
          for line = (read-line stream nil nil)
          while line
          do (let* ((parts (split-string line #\,))
                    (x (parse-integer (first parts)))
                    (y (parse-integer (second parts))))
               (when (and (>= x 0) (< x +size+)
                          (>= y 0) (< y +size+))
                 (setf (aref *grid* y x) t))))))

(defun main ()
  (read-input "input.txt")

  (enqueue (list 0 0 0))
  (setf (aref *visited* 0 0) t)

  (loop
    (when (queue-empty-p)
      (format t "No path~%")
      (return))

    (let* ((current (dequeue))
           (x (first current))
           (y (second current))
           (steps (third current)))

      (when (and (= x (- +size+ 1)) (= y (- +size+ 1)))
        (format t "~a~%" steps)
        (return))

      (dolist (dir *dirs*)
        (let* ((dx (first dir))
               (dy (second dir))
               (nx (+ x dx))
               (ny (+ y dy)))

          (when (and (>= nx 0) (< nx +size+)
                     (>= ny 0) (< ny +size+)
                     (not (aref *grid* ny nx))
                     (not (aref *visited* ny nx)))
            (setf (aref *visited* ny nx) t)
            (enqueue (list nx ny (+ steps 1)))))))))

(main)
