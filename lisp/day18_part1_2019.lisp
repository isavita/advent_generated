
(declaim (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))

(defstruct (point (:constructor make-point (x y)))
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(defstruct (state (:constructor make-state (pos keys)))
  (pos (make-point 0 0) :type point)
  (keys 0 :type fixnum))

(defstruct (queue (:constructor make-queue (&optional in out)))
  (in nil :type list)
  (out nil :type list))

(defun queue-empty-p (q)
  (and (null (queue-in q)) (null (queue-out q))))

(defun queue-enqueue (item q)
  (push item (queue-in q))
  q)

(defun queue-dequeue (q)
  (when (null (queue-out q))
    (when (null (queue-in q))
      (return-from queue-dequeue nil))
    (setf (queue-out q) (nreverse (queue-in q)))
    (setf (queue-in q) nil))
  (pop (queue-out q)))

(defun find-shortest-path (grid start key-map height width)
  (let* ((dirs (list (make-point 0 -1) (make-point -1 0)
                     (make-point 0 1) (make-point 1 0)))
         (visited (make-hash-table :test 'equal))
         (queue (make-queue))
         (steps 0)
         (all-keys-mask (1- (ash 1 (hash-table-count key-map)))))

    (queue-enqueue (make-state start 0) queue)
    (setf (gethash (list (point-x start) (point-y start) 0) visited) t)

    (loop while (not (queue-empty-p queue))
          do (let ((level-size (+ (length (queue-in queue)) (length (queue-out queue)))))
                   (loop for i fixnum from 0 below level-size
                         for current = (queue-dequeue queue)
                         while current
                         do (when (= (state-keys current) all-keys-mask)
                              (return-from find-shortest-path steps))

                            (loop for d in dirs
                                  do (let* ((next-x (+ (point-x (state-pos current)) (point-x d)))
                                            (next-y (+ (point-y (state-pos current)) (point-y d))))
                                       (when (and (>= next-x 0) (< next-x width)
                                                  (>= next-y 0) (< next-y height))
                                         (let ((char (char (aref grid next-y) next-x)))
                                           (when (and (not (char= char #\#))
                                                      (or (not (and (char>= char #\A) (char<= char #\Z)))
                                                          (/= (logand (state-keys current)
                                                                      (ash 1 (gethash (char-downcase char) key-map)))
                                                              0)))
                                             (let* ((new-keys (state-keys current))
                                                    (new-pos (make-point next-x next-y)))
                                               (when (and (char>= char #\a) (char<= char #\z))
                                                 (setf new-keys (logior new-keys (ash 1 (gethash char key-map)))))
                                               (let ((state-key (list next-x next-y new-keys)))
                                                 (unless (gethash state-key visited)
                                                   (setf (gethash state-key visited) t)
                                                   (queue-enqueue (make-state new-pos new-keys) queue))))))))))
                   (incf steps)))
    -1))

(defun main ()
  (let* ((grid-lines-rev nil)
         (start nil)
         (key-map (make-hash-table :test 'eql))
         (key-counter 0))

    (with-open-file (stream "input.txt" :direction :input)
      (loop for line = (read-line stream nil nil)
            for y from 0
            while line
            do (push line grid-lines-rev)
               (loop for char across line
                     for x from 0
                     do (cond ((char= char #\@)
                               (setf start (make-point x y)))
                              ((and (char>= char #\a) (char<= char #\z))
                               (setf (gethash char key-map) key-counter)
                               (incf key-counter))))))

    (let* ((grid (make-array (length grid-lines-rev) :initial-contents (nreverse grid-lines-rev) :element-type 'string))
           (height (length grid))
           (width (length (aref grid 0))))
      (format t "~a~%" (find-shortest-path grid start key-map height width)))))

(main)
