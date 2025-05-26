
(defun split-string (s delimiter)
  (loop for i = 0 then (1+ j)
        as j = (position delimiter s :start i)
        collect (subseq s i (or j (length s)))
        while j))

(defun permutations (lst)
  (if (null lst)
      '(())
      (loop for x in lst
            nconc (loop for p in (permutations (remove x lst :count 1))
                         collect (cons x p)))))

(defun main ()
  (let ((distances (make-hash-table :test 'equal))
        (locations (make-hash-table :test 'equal)))

    (with-open-file (file "input.txt" :direction :input)
      (loop for line = (read-line file nil nil)
            while line do
              (let* ((parts (split-string line #\Space))
                     (loc1 (nth 0 parts))
                     (loc2 (nth 2 parts))
                     (dist (parse-integer (nth 4 parts))))
                (setf (gethash (list loc1 loc2) distances) dist)
                (setf (gethash (list loc2 loc1) distances) dist)
                (setf (gethash loc1 locations) t)
                (setf (gethash loc2 locations) t))))

    (let* ((location-list (loop for k being the hash-key of locations collect k))
           (shortest-distance most-positive-fixnum)
           (longest-distance 0))

      (loop for perm in (permutations location-list) do
        (let ((total-distance 0))
          (loop for current-pair on perm
                while (cdr current-pair)
                do
                   (let ((city1 (car current-pair))
                         (city2 (cadr current-pair)))
                     (incf total-distance (gethash (list city1 city2) distances))))
          (setf shortest-distance (min shortest-distance total-distance))
          (setf longest-distance (max longest-distance total-distance))))

      (format t "~a~%" shortest-distance)
      (format t "~a~%" longest-distance))))

(main)
