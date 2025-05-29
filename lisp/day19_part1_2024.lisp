
(defun split-string (string delimiter)
  (loop for start = 0 then (1+ end)
        for end = (position delimiter string :start start)
        collect (string-trim '(#\Space #\Tab #\Newline #\Return) (subseq string start (or end (length string))))
        while end))

(defun read-lines (filename)
  (with-open-file (stream filename :direction :input :if-does-not-exist :error)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(defun can-make (design patterns-hash-table)
  (let* ((n (length design))
         (dp (make-array (1+ n) :initial-element nil)))
    (setf (aref dp 0) t)

    (loop for i from 1 to n do
      (loop for j from 1 to i do
        (let ((sub (subseq design (- i j) i)))
          (when (and (aref dp (- i j))
                     (gethash sub patterns-hash-table))
            (setf (aref dp i) t)
            (return)))))
    (aref dp n)))

(defun main ()
  (let* ((input-lines (read-lines "input.txt"))
         (available-patterns-raw (first input-lines))
         (available-patterns-list (split-string available-patterns-raw #\,))
         (patterns-hash (make-hash-table :test 'equal)))

    (loop for p in available-patterns-list do
      (setf (gethash p patterns-hash) t))

    (let ((count 0))
      (loop for i from 2 below (length input-lines) do
        (let* ((design-raw (nth i input-lines))
               (design (string-trim '(#\Space #\Tab #\Newline #\Return) design-raw)))
          (when (can-make design patterns-hash)
            (incf count))))
      (format t "~a~%" count))))

(main)
