
(defun char-whitespace-p (c)
  (member c '(#\Space #\Tab #\Newline #\Return) :test 'char=))

(defun parse-line (line)
  (let* ((trimmed-line (string-trim '(#\Space #\Tab #\Newline #\Return) line))
         (parts nil)
         (len (length trimmed-line)))
    (loop with start = 0
          for non-space-start = (position-if-not #'char-whitespace-p trimmed-line :start start)
          do (when (null non-space-start) (loop-finish))
             (let* ((space-end (position-if #'char-whitespace-p trimmed-line :start non-space-start))
                    (word-end (or space-end len)))
               (push (subseq trimmed-line non-space-start word-end) parts)
               (setf start (if space-end space-end len)))
          until (>= start len))
    (nreverse parts)))

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
            while line
            do (let* ((parts (parse-line line))
                      (loc1 (nth 0 parts))
                      (loc2 (nth 2 parts))
                      (dist (parse-integer (nth 4 parts))))
                 (setf (gethash (list loc1 loc2) distances) dist)
                 (setf (gethash (list loc2 loc1) distances) dist)
                 (setf (gethash loc1 locations) t)
                 (setf (gethash loc2 locations) t))))

    (let* ((all-locations (loop for loc being the hash-key of locations collect loc))
           (min-distance most-positive-fixnum))

      (loop for perm in (permutations all-locations)
            do (let ((current-distance 0))
                 (loop with prev-loc = (first perm)
                       for current-loc in (rest perm)
                       do (incf current-distance (gethash (list prev-loc current-loc) distances))
                          (setf prev-loc current-loc))
                 (setf min-distance (min min-distance current-distance))))
      (format t "~a~%" min-distance))))

(main)
