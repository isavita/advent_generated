
(defun string-split (string delimiter)
  (let ((parts (list)))
    (loop with start = 0
          for pos = (search delimiter string :start2 start)
          do (push (subseq string start (or pos (length string))) parts)
          when (null pos) do (return)
          do (setf start (+ pos (length delimiter))))
    (nreverse parts)))

(defvar *adj* (make-hash-table :test 'eql))
(defvar *visited* (make-hash-table :test 'eql))

(defun parse-line (line)
  (let* ((stripped-line (string-trim '(#\Space #\Tab #\Newline) line))
         (parts (string-split stripped-line " <-> "))
         (from-node (parse-integer (first parts)))
         (to-nodes-str (second parts))
         (to-nodes (mapcar #'parse-integer
                           (string-split to-nodes-str ", "))))
    (unless (gethash from-node *adj*)
      (setf (gethash from-node *adj*) nil))
    (loop for to-node in to-nodes
          do (push to-node (gethash from-node *adj*))
             (unless (gethash to-node *adj*)
               (setf (gethash to-node *adj*) nil))
             (push from-node (gethash to-node *adj*)))))

(defun dfs (node)
  (setf (gethash node *visited*) t)
  (loop for neighbor in (gethash node *adj*)
        when (not (gethash neighbor *visited*))
          do (dfs neighbor)))

(defun main ()
  (with-open-file (stream "input.txt" :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          do (parse-line line)))
  (let ((groups 0))
    (loop for node being the hash-key in *adj*
          when (not (gethash node *visited*))
            do (dfs node)
               (incf groups))
    (format t "~a~%" groups)))

(main)
