
(defun split-string (s delimiter)
  (loop for i = 0 then (1+ j)
        for j = (position delimiter s :start i)
        collect (subseq s i j)
        while j))

(defun read-input (filepath)
  (with-open-file (f filepath :direction :input)
    (loop for line = (read-line f nil)
          while line
          collect (split-string line #\-))))

(defun build-graph (edges)
  (let ((graph (make-hash-table :test 'equal)))
    (dolist (edge edges graph)
      (let ((from (first edge))
            (to (second edge)))
        (push to (gethash from graph nil))
        (push from (gethash to graph nil))))))

(defun is-small-cave (cave)
  (string= cave (string-downcase cave)))

(defun count-paths (graph current visited)
  (cond
    ((string= current "end") 1)
    (t
     (when (is-small-cave current)
       (setf (gethash current visited) t))
     (let ((total-paths 0))
       (dolist (neighbor (gethash current graph nil))
         (unless (gethash neighbor visited)
           (incf total-paths (count-paths graph neighbor visited))))
       (when (is-small-cave current)
         (remhash current visited))
       total-paths))))

(defun main ()
  (let* ((edges (read-input "input.txt"))
         (graph (build-graph edges))
         (total-paths (count-paths graph "start" (make-hash-table :test 'equal))))
    (print total-paths)))

(main)
