
(defun reverse-string (s)
  (let* ((len (length s))
         (new-s (make-string len)))
    (loop for i from 0 below len
          do (setf (char new-s i) (char s (- (1- len) i))))
    new-s))

(defun split-by-slash (s)
  (loop for start = 0 then (1+ end)
        for end = (position #\/ s :start start)
        collect (subseq s start (or end (length s)))
        while end))

(defun split-by-string (s delim)
  (let ((pos (search delim s)))
    (if pos
        (list (subseq s 0 pos) (subseq s (+ pos (length delim))))
        (list s))))

(defun rotate-pattern (pattern-str)
  (let* ((parts (split-by-slash pattern-str))
         (size (length parts))
         (new-parts (make-list size)))
    (loop for new-row-idx from 0 below size
          do (let ((current-new-row (make-string size)))
               (loop for new-col-idx from 0 below size
                     for old-y = (- (1- size) new-col-idx)
                     for old-x = new-row-idx
                     do (setf (char current-new-row new-col-idx) (char (nth old-y parts) old-x)))
               (setf (nth new-row-idx new-parts) current-new-row)))
    (format nil "~{~A~^/~}" new-parts)))

(defun flip-pattern (pattern-str)
  (let ((parts (split-by-slash pattern-str)))
    (format nil "~{~A~^/~}" (mapcar #'reverse-string parts))))

(defun get-all-transforms (pattern-str)
  (let ((transforms '()))
    (loop with current-pattern = pattern-str
          for i from 0 below 4
          do (push current-pattern transforms)
             (setf current-pattern (rotate-pattern current-pattern)))
    (loop with current-pattern = (flip-pattern pattern-str)
          for i from 0 below 4
          do (push current-pattern transforms)
             (setf current-pattern (rotate-pattern current-pattern)))
    transforms))

(defun main ()
  (let ((rules (make-hash-table :test 'equal))
        (grid (vector ".#." "..#" "###")))

    (with-open-file (file "input.txt" :direction :input)
      (loop for line = (read-line file nil)
            while line
            do (let* ((parts (split-by-string line " => "))
                      (key (first parts))
                      (value (second parts)))
                 (loop for transformed-key in (get-all-transforms key)
                       do (setf (gethash transformed-key rules) value)))))

    (loop for i from 0 below 5
          do (let* ((current-grid-len (length grid))
                    (sub-size (if (zerop (mod current-grid-len 2)) 2 3))
                    (enhanced-block-size (if (= sub-size 2) 3 4))
                    (num-blocks-per-side (floor current-grid-len sub-size))
                    (new-grid-size (* num-blocks-per-side enhanced-block-size))
                    (temp-rows (make-array new-grid-size :initial-element nil)))

               (loop for y-block from 0 below num-blocks-per-side
                     for y-orig = (* y-block sub-size)
                     do (loop for x-block from 0 below num-blocks-per-side
                              for x-orig = (* x-block sub-size)
                              do (let* ((square-parts (loop for dy from 0 below sub-size
                                                             collect (subseq (aref grid (+ y-orig dy))
                                                                             x-orig (+ x-orig sub-size))))
                                        (square-key (format nil "~{~A~^/~}" square-parts))
                                        (enhanced-square-str (gethash square-key rules)))
                                   (loop for dy from 0 below enhanced-block-size
                                         for output-row-idx = (+ (* y-block enhanced-block-size) dy)
                                         for enhanced-row-part = (nth dy (split-by-slash enhanced-square-str))
                                         do (setf (aref temp-rows output-row-idx)
                                                  (append (aref temp-rows output-row-idx) (list enhanced-row-part)))))))

               (setf grid (loop for row-parts across temp-rows
                                collect (apply #'concatenate 'string row-parts)))
               (setf grid (make-array (length grid) :initial-contents grid))))

    (let ((count 0))
      (loop for row across grid
            do (incf count (count #\# row)))
      (princ count))))

(main)
