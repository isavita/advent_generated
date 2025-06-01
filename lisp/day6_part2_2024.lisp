
(defun loops (grid start-x start-y start-dir)
  (let* ((h (array-dimension grid 0))
         (w (array-dimension grid 1))
         (dirs '((0 -1) (1 0) (0 1) (-1 0)))
         (x start-x)
         (y start-y)
         (dir-idx start-dir)
         (seen (make-hash-table :test 'equal)))
    (loop for step from 0 below 2000000 do
      (let ((current-state (list x y dir-idx)))
        (when (gethash current-state seen)
          (return-from loops t))
        (setf (gethash current-state seen) t))
      (let* ((dir-vec (nth dir-idx dirs))
             (dir-x (first dir-vec))
             (dir-y (second dir-vec))
             (nx (+ x dir-x))
             (ny (+ y dir-y)))
        (when (or (< nx 0) (>= nx w) (< ny 0) (>= ny h))
          (return-from loops nil))
        (if (char= (aref grid ny nx) #\#)
            (setf dir-idx (mod (+ dir-idx 1) 4))
            (setf x nx
                  y ny))))))

(defun split-string-by-newline (s)
  (let ((result nil)
        (start 0))
    (loop
      (let ((end (position #\Newline s :start start)))
        (push (subseq s start (or end (length s))) result)
        (unless end (return))
        (setf start (1+ end))))
    (nreverse result)))

(defun find-loop-positions (input-string)
  (let* ((all-lines (split-string-by-newline input-string))
         (lines (remove-if (lambda (s) (string= (string-trim '(#\Space #\Tab #\Newline #\Return) s) ""))
                           (mapcar (lambda (s) (string-trim '(#\Space #\Tab #\Newline #\Return) s))
                                   all-lines)))
         (h (length lines))
         (w (if (null lines) 0 (length (first lines))))
         (grid (make-array (list h w) :element-type 'character))
         (start-x nil)
         (start-y nil)
         (start-dir nil))
    (loop for i from 0 below h
          for line in lines do
      (loop for j from 0 below w
            for char = (char line j) do
        (setf (aref grid i j) char)
        (cond
          ((char= char #\^) (setf start-x j start-y i start-dir 0))
          ((char= char #\>) (setf start-x j start-y i start-dir 1))
          ((char= char #\v) (setf start-x j start-y i start-dir 2))
          ((char= char #\<) (setf start-x j start-y i start-dir 3)))))
    (setf (aref grid start-y start-x) #\.)
    (let ((can-loop 0))
      (loop for y from 0 below h do
        (loop for x from 0 below w do
          (unless (and (= x start-x) (= y start-y))
            (when (char= (aref grid y x) #\.)
              (setf (aref grid y x) #\#)
              (when (loops grid start-x start-y start-dir)
                (incf can-loop))
              (setf (aref grid y x) #\.)))))
      can-loop)))

(defun main ()
  (let ((input-string
          (with-open-file (f "input.txt" :direction :input :element-type :default)
            (let ((content (make-string (file-length f))))
              (read-sequence content f)
              content))))
    (print (find-loop-positions input-string))))

(main)
