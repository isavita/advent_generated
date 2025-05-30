
(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          when (not (string= line ""))
            collect (parse-integer line))))

(defun calculate-differences (adapters)
  (let* ((sorted-adapters (sort (copy-list adapters) #'<))
         (max-adapter (if sorted-adapters (car (last sorted-adapters)) 0))
         (padded-adapters (coerce (cons 0 (append sorted-adapters (list (+ max-adapter 3)))) 'vector))
         (one-jolt-diffs 0)
         (three-jolt-diffs 0))
    (loop for i from 1 below (length padded-adapters)
          do (let* ((current (aref padded-adapters i))
                    (prev (aref padded-adapters (- i 1)))
                    (diff (- current prev)))
               (case diff
                 (1 (incf one-jolt-diffs))
                 (3 (incf three-jolt-diffs)))))
    (list one-jolt-diffs three-jolt-diffs)))

(defun count-arrangements (adapters)
  (let* ((sorted-adapters (sort (copy-list adapters) #'<))
         (padded-adapters (coerce (cons 0 sorted-adapters) 'vector))
         (n (length padded-adapters))
         (ways (make-array n :initial-element 0)))
    (setf (aref ways 0) 1)
    (loop for i from 1 below n
          do (loop for j from 1 to 3
                   do (let ((prev-idx (- i j)))
                        (when (and (>= prev-idx 0)
                                   (<= (- (aref padded-adapters i) (aref padded-adapters prev-idx)) 3))
                          (incf (aref ways i) (aref ways prev-idx))))))
    (aref ways (- n 1))))

(defun main ()
  (let* ((adapters (read-input "input.txt"))
         (diffs (calculate-differences adapters))
         (one-jolt-diffs (car diffs))
         (three-jolt-diffs (cadr diffs))
         (part-one-result (* one-jolt-diffs three-jolt-diffs)))
    (format t "Part One: ~a~%" part-one-result)
    (let ((arrangements (count-arrangements adapters)))
      (format t "Part Two: ~a~%" arrangements))))

(main)
