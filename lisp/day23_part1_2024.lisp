
(defun string-split (string delimiter)
  (loop for start = 0 then (1+ end)
        for end = (position delimiter string :start start)
        collect (subseq string start (or end (length string)))
        while end))

(defun starts-with-t (s)
  (and (> (length s) 0) (char= (char s 0) #\t)))

(defun solve ()
  (let ((graph (make-hash-table :test 'equal))
        (seen-triplets (make-hash-table :test 'equal))
        (count 0))

    (with-open-file (in "input.txt" :direction :input :if-does-not-exist :error)
      (loop for line = (read-line in nil nil)
            while line
            do (let ((parts (string-split line #\-)))
                 (when (= (length parts) 2)
                   (let ((from (first parts))
                         (to (second parts)))
                     (unless (gethash from graph)
                       (setf (gethash from graph) (make-hash-table :test 'equal)))
                     (unless (gethash to graph)
                       (setf (gethash to graph) (make-hash-table :test 'equal)))
                     (setf (gethash to (gethash from graph)) t)
                     (setf (gethash from (gethash to graph)) t))))))

    (let* ((computers (loop for k being the hash-keys of graph collect k))
           (n (length computers)))
      (loop for i from 0 below n
            do (loop for j from (1+ i) below n
                     do (loop for k from (1+ j) below n
                              do (let* ((c1 (nth i computers))
                                         (c2 (nth j computers))
                                         (c3 (nth k computers))
                                         (neighbors-c1 (gethash c1 graph))
                                         (neighbors-c2 (gethash c2 graph))
                                         (neighbors-c3 (gethash c3 graph)))
                                   (when (and neighbors-c1 neighbors-c2 neighbors-c3
                                              (gethash c2 neighbors-c1)
                                              (gethash c3 neighbors-c1)
                                              (gethash c3 neighbors-c2))
                                     (when (or (starts-with-t c1)
                                               (starts-with-t c2)
                                               (starts-with-t c3))
                                       (let* ((triplet-list (sort (list c1 c2 c3) #'string<))
                                              (triplet-string (format nil "~a,~a,~a"
                                                                      (first triplet-list)
                                                                      (second triplet-list)
                                                                      (third triplet-list))))
                                         (unless (gethash triplet-string seen-triplets)
                                           (setf (gethash triplet-string seen-triplets) t)
                                           (incf count))))))))))
    (format t "Number of triplets containing at least one computer with name starting with 't': ~a~%" count)))

(defun main ()
  (solve))

(main)
