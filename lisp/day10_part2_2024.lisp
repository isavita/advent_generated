
(defun main ()
  (let* ((filename "input.txt")
         (lines (with-open-file (f filename :direction :input)
                  (loop for line = (read-line f nil)
                        while line collect line)))
         (nr (length lines))
         (nc (length (first lines)))
         (grid (make-array (list nr nc) :element-type 'fixnum))
         (dp (make-array (list nr nc) :initial-element -1))
         (dirs '((1 . 0) (-1 . 0) (0 . 1) (0 . -1))))
    (loop for r from 0 below nr
          do (loop for c from 0 below nc
                   do (setf (aref grid r c)
                            (- (char-code (aref (nth r lines) c)) (char-code #\0)))))
    (labels ((dfs (r c)
               (let ((memo-val (aref dp r c)))
                 (if (/= memo-val -1)
                     memo-val
                     (let ((h (aref grid r c)))
                       (if (= h 9)
                           (progn (setf (aref dp r c) 1) 1)
                           (let ((sum-paths 0))
                             (dolist (dir dirs)
                               (let* ((dr (car dir))
                                      (dc (cdr dir))
                                      (nr2 (+ r dr))
                                      (nc2 (+ c dc)))
                                 (when (and (>= nr2 0) (< nr2 nr)
                                            (>= nc2 0) (< nc2 nc)
                                            (= (aref grid nr2 nc2) (+ h 1)))
                                   (incf sum-paths (dfs nr2 nc2)))))
                             (setf (aref dp r c) sum-paths)
                             sum-paths)))))))
      (let ((total 0))
        (loop for r from 0 below nr
              do (loop for c from 0 below nc
                       do (when (= (aref grid r c) 0)
                            (incf total (dfs r c)))))
        (format t "~a~%" total)))))

(main)
