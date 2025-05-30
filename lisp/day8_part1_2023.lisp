
(defconstant +elem-to-match+ "ZZZ")
(defconstant +start-node+ "AAA")
(defconstant +input-file+ "input.txt")

(defun read-lines-from-file (filename)
  (with-open-file (stream filename :direction :input :if-does-not-exist :error)
    (loop for line = (read-line stream nil :eof)
          until (eq line :eof)
          collect (string-trim '(#\Space #\Tab #\Newline #\Return) line))))

(defun main ()
  (let* ((lines (read-lines-from-file +input-file+))
         (instructions (first lines))
         (desert-map (make-hash-table :test 'equal)))
    (loop for line in (cddr lines)
          when (string-trim '(#\Space #\Tab #\Newline #\Return) line)
          do (let ((node (subseq line 0 3))
                   (left (subseq line 7 10))
                   (right (subseq line 12 15)))
               (setf (gethash node desert-map) (list left right))))
    (let ((current +start-node+)
          (steps 0))
      (loop while (string/= current +elem-to-match+)
        do (loop for char across instructions
                 do (incf steps)
                    (let ((directions (gethash current desert-map)))
                      (cond ((char= char #\R)
                             (setf current (second directions)))
                            ((char= char #\L)
                             (setf current (first directions)))))
                    (when (string= current +elem-to-match+)
                      (return))))
      (print steps))))

(main)
