
(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))

(defstruct (queue (:constructor make-queue (&optional (in nil) (out nil))))
  in out)

(defun enqueue (item q)
  (setf (queue-in q) (cons item (queue-in q)))
  q)

(defun dequeue (q)
  (when (null (queue-out q))
    (setf (queue-out q) (nreverse (queue-in q)))
    (setf (queue-in q) nil))
  (pop (queue-out q)))

(defun queue-empty-p (q)
  (and (null (queue-in q)) (null (queue-out q))))

(defun read-input (filepath)
  (with-open-file (stream filepath :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect (string-trim '(#\Space #\Tab #\Newline #\Return) line))))

(defun parse-map (lines)
  (let* ((height (length lines))
         (width (length (car lines)))
         (map (make-array (list height width) :element-type 'character))
         (points (make-hash-table :test 'equal)))
    (loop for y from 0 below height
          for line in lines
          do (loop for x from 0 below width
                   for char = (char line x)
                   do (setf (aref map y x) char)
                      (when (digit-char-p char)
                        (setf (gethash (string char) points) (list x y)))))
    (values map points)))

(defun point-key (point)
  (format nil "~a,~a" (car point) (cadr point)))

(defun bfs (map start-point end-point)
  (let* ((q (make-queue))
         (visited (make-hash-table :test 'equal))
         (map-height (array-dimension map 0))
         (map-width (array-dimension map 1))
         (directions '((0 1) (0 -1) (1 0) (-1 0))))

    (enqueue (list start-point 0) q)
    (setf (gethash (point-key start-point) visited) t)

    (loop until (queue-empty-p q)
          do (let* ((current-node (dequeue q))
                    (current-point (car current-node))
                    (steps (cadr current-node))
                    (cx (car current-point))
                    (cy (cadr current-point)))

               (when (and (= cx (car end-point))
                          (= cy (cadr end-point)))
                 (return-from bfs steps))

               (loop for dir in directions
                     do (let* ((dx (car dir))
                                (dy (cadr dir))
                                (nx (+ cx dx))
                                (ny (+ cy dy))
                                (next-point (list nx ny))
                                (next-key (point-key next-point)))

                          (when (and (>= nx 0) (< nx map-width)
                                     (>= ny 0) (< ny map-height)
                                     (char/= (aref map ny nx) #\#)
                                     (not (gethash next-key visited)))
                            (setf (gethash next-key visited) t)
                            (enqueue (list next-point (+ steps 1)) q))))))
    -1))

(defun generate-permutations (elements)
  (labels ((permute-rec (current-list remaining-elements)
             (if (null remaining-elements)
                 (list current-list)
                 (loop for element in remaining-elements
                       append (permute-rec (append current-list (list element))
                                           (remove element remaining-elements :test 'equal))))))
    (permute-rec nil elements)))


(defun find-shortest-path (map all-points)
  (let* ((point-keys-raw (loop for k being the hash-keys of all-points collect k))
         (point-keys (sort (remove "0" point-keys-raw :test 'equal) #'string<))
         (n (length point-keys))
         (adj-matrix (make-array (list n n) :element-type 'fixnum :initial-element 0))
         (start-paths (make-array n :element-type 'fixnum :initial-element 0))
         (start-point (gethash "0" all-points))
         (min-path-length most-positive-fixnum))

    (loop for i from 0 below n
          for pk-i = (nth i point-keys)
          for pt-i = (gethash pk-i all-points)
          do (setf (aref start-paths i) (the fixnum (bfs map start-point pt-i)))
             (loop for j from 0 below n
                   for pk-j = (nth j point-keys)
                   for pt-j = (gethash pk-j all-points)
                   when (/= i j)
                   do (setf (aref adj-matrix i j) (the fixnum (bfs map pt-i pt-j)))))

    (let ((indices (loop for i from 0 below n collect i)))
      (loop for p in (generate-permutations indices)
            do (let ((current-path-length (the fixnum (aref start-paths (car p)))))
                 (loop for i from 1 below n
                       do (incf current-path-length (the fixnum (aref adj-matrix (nth (- i 1) p) (nth i p)))))
                 (setf min-path-length (min min-path-length current-path-length)))))

    min-path-length))

(defun main ()
  (let* ((filepath "input.txt")
         (lines (read-input filepath)))
    (multiple-value-bind (map points) (parse-map lines)
      (let ((shortest-path (find-shortest-path map points)))
        (format t "~a~%" shortest-path)))))

(main)
