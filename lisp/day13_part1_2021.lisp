
(defun string-starts-with (prefix string)
  (and (>= (length string) (length prefix))
       (string= prefix string :end2 (length prefix))))

(defun split-string (string delimiter)
  (loop for i = 0 then (1+ j)
        for j = (position delimiter string :start i)
        collect (subseq string i (or j (length string)))
        while j))

(defun read-input (filepath)
  (let ((dots '())
        (folds '()))
    (with-open-file (stream filepath :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            do (cond ((string= line "") nil)
                     ((string-starts-with "fold along " line)
                      (push (subseq line (length "fold along ")) folds))
                     (t
                      (let* ((parts (split-string line #\,))
                             (x (parse-integer (first parts)))
                             (y (parse-integer (second parts))))
                        (push (list x y) dots))))))
    (list (nreverse dots) (nreverse folds))))

(defun fold-dots (dots fold-instruction)
  (let* ((parts (split-string fold-instruction #\=))
         (axis (char (first parts) 0))
         (fold-value (parse-integer (second parts)))
         (unique-dots (make-hash-table :test #'equal)))

    (dolist (dot dots)
      (let* ((x (first dot))
             (y (second dot))
             (new-x x)
             (new-y y))
        (cond ((and (char= axis #\y) (> y fold-value))
               (setf new-y (- (* 2 fold-value) y)))
              ((and (char= axis #\x) (> x fold-value))
               (setf new-x (- (* 2 fold-value) x))))
        (setf (gethash (list new-x new-y) unique-dots) t)))

    (hash-table-count unique-dots)))

(defun main ()
  (let* ((input-data (read-input "input.txt"))
         (initial-dots (first input-data))
         (folds (second input-data))
         (first-fold (first folds)))
    (princ (fold-dots initial-dots first-fold))))

(main)
