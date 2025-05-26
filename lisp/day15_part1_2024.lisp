
(defun solve ()
  (let* (grid-lines
         (moves-str "")
         (reading-map t))
    (with-open-file (f "input.txt" :direction :input)
      (loop for line = (read-line f nil)
            while line
            do (if reading-map
                   (if (find #\# line)
                       (push line grid-lines)
                       (progn
                         (setf reading-map nil)
                         (setf moves-str (concatenate 'string moves-str line))))
                   (setf moves-str (concatenate 'string moves-str line)))))
    (setf grid-lines (nreverse grid-lines))
    (let* ((rows (length grid-lines))
           (cols (length (car grid-lines)))
           (grid (make-array (list rows cols) :element-type 'character))
           (robot-r 0)
           (robot-c 0)
           (dirs (make-hash-table :test 'eql)))
      (loop for r from 0 below rows
            for line in grid-lines
            do (loop for c from 0 below cols
                     do (setf (aref grid r c) (char line c))))
      (setf (gethash #\^ dirs) (list -1 0))
      (setf (gethash #\v dirs) (list 1 0))
      (setf (gethash #\< dirs) (list 0 -1))
      (setf (gethash #\> dirs) (list 0 1))
      (loop for r from 0 below rows
            do (loop for c from 0 below cols
                     do (if (char= (aref grid r c) #\@)
                            (progn
                              (setf robot-r r)
                              (setf robot-c c)
                              (return-from nil)))))
      (labels ((push-boxes (r c dr dc)
                 (let* ((nr (+ r dr))
                        (nc (+ c dc))
                        (next-char (aref grid nr nc)))
                   (cond ((char= next-char #\#) nil)
                         ((char= next-char #\O)
                          (if (push-boxes nr nc dr dc)
                              (progn (setf (aref grid nr nc) #\O) t)
                              nil))
                         ((char= next-char #\.)
                          (setf (aref grid nr nc) #\O)
                          (setf (aref grid r c) #\.)
                          t)
                         (t nil)))))
        (loop for move-char across moves-str
              do (let* ((dir-vec (gethash move-char dirs))
                        (dr (car dir-vec))
                        (dc (cadr dir-vec))
                        (nr (+ robot-r dr))
                        (nc (+ robot-c dc))
                        (next-char (aref grid nr nc)))
                   (cond ((char= next-char #\#) nil)
                         ((char= next-char #\O)
                          (if (push-boxes nr nc dr dc)
                              (progn
                                (setf (aref grid robot-r robot-c) #\.)
                                (setf (aref grid nr nc) #\@)
                                (setf robot-r nr)
                                (setf robot-c nc))
                              nil))
                         ((char= next-char #\.)
                          (setf (aref grid robot-r robot-c) #\.)
                          (setf (aref grid nr nc) #\@)
                          (setf robot-r nr)
                          (setf robot-c nc))
                         (t nil)))))
      (let ((total-sum 0))
        (loop for r from 0 below rows
              do (loop for c from 0 below cols
                       do (if (char= (aref grid r c) #\O)
                              (incf total-sum (+ (* r 100) c)))))
        (print total-sum)))))

(defun main ()
  (solve))

(main)
