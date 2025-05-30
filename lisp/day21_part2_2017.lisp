
(defun string-split-by-char (string delimiter)
  (loop with start = 0
        for end = (position delimiter string :start start)
        collect (subseq string start (if end end (length string)))
        while end
        do (setf start (1+ end))))

(defun string-split-by-string (string delimiter)
  (let ((delimiter-length (length delimiter)))
    (loop with results = '()
          for start = 0 then (+ end delimiter-length)
          for end = (search delimiter string :start2 start)
          do (push (subseq string start end) results)
          while end
          finally (return (nreverse results)))))

(defun read-file-contents (filename)
  (with-open-file (stream filename :direction :input :if-does-not-exist :error)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun parse-input (data)
  (loop for line in (string-split-by-char data #\Newline)
        when (string/= line "")
          collect (destructuring-bind (input output) (string-split-by-string line " => ")
                    (cons input output))))

(defun split-pattern (pattern)
  (string-split-by-char pattern #\/))

(defun join-pattern (rows)
  (format nil "~{~A~^/~}" rows))

(defun rotate-grid (grid)
  (let ((size (length grid)))
    (loop for col from 0 below size
          collect (coerce (loop for row from (- size 1) downto 0
                                collect (elt (nth row grid) col))
                          'string))))

(defun flip-grid (grid)
  (reverse grid))

(defun get-all-transformations (pattern)
  (let* ((grid (split-pattern pattern))
         (transformations '())
         (current-grid grid))
    (loop for i from 0 below 4 do
      (push (join-pattern current-grid) transformations)
      (setf current-grid (rotate-grid current-grid)))

    (let* ((flipped-grid (flip-grid grid))
           (current-flipped-grid flipped-grid))
      (loop for i from 0 below 4 do
        (push (join-pattern current-flipped-grid) transformations)
        (setf current-flipped-grid (rotate-grid current-flipped-grid))))
    (nreverse (remove-duplicates transformations :test 'string=))))

(defun enhance (grid rule-map)
  (let* ((size (length grid))
         (step (if (zerop (mod size 2)) 2 3))
         (new-block-size (+ step 1))
         (num-blocks (/ size step))
         (new-size (* num-blocks new-block-size))
         (new-grid (make-array new-size :initial-element "" :element-type 'string)))

    (loop for i-block from 0 below num-blocks
          do (loop for j-block from 0 below num-blocks
                   do (let* ((start-row (* i-block step))
                             (start-col (* j-block step))
                             (block-rows (loop for k from start-row below (+ start-row step)
                                               collect (subseq (nth k grid) start-col (+ start-col step))))
                             (block-pattern (join-pattern block-rows))
                             (output-pattern (gethash block-pattern rule-map))
                             (output-rows (split-pattern output-pattern)))
                        (loop for k from 0 below new-block-size
                              do (setf (aref new-grid (+ (* i-block new-block-size) k))
                                       (concatenate 'string
                                                    (aref new-grid (+ (* i-block new-block-size) k))
                                                    (nth k output-rows)))))))
    (coerce new-grid 'list)))

(defun count-on-pixels (grid)
  (loop for row in grid
        sum (count #\# row)))

(defun main ()
  (let* ((rules-data (read-file-contents "input.txt"))
         (parsed-rules (parse-input rules-data))
         (rule-map (make-hash-table :test 'equal)))

    (loop for (input . output) in parsed-rules
          do (loop for tx in (get-all-transformations input)
                   do (setf (gethash tx rule-map) output)))

    (let ((grid '(".#." "..#" "###")))
      (loop for i from 0 below 18 do
        (setf grid (enhance grid rule-map)))

      (format t "~a~%" (count-on-pixels grid)))))

(main)
