
(defun small-cave-p (cave)
  (loop for char across cave
        always (lower-case-p char)))

(defun split-string (s delimiter)
  (loop for start = 0 then (1+ end)
        for end = (position delimiter s :start start)
        collect (subseq s start (or end (length s)))
        while end))

(defun find-paths (graph start end visited-caves small-twice-used)
  (cond
    ((string= start end) 1)
    (t
     (let ((next-small-twice-used small-twice-used))
       (when (small-cave-p start)
         (when (member start visited-caves :test #'string=)
           (if next-small-twice-used
               (return-from find-paths 0)
               (setf next-small-twice-used t))))
       (let ((next-visited-caves (if (small-cave-p start)
                                     (adjoin start visited-caves :test #'string=)
                                     visited-caves)))
         (loop for neighbor in (gethash start graph)
               sum (find-paths graph neighbor end next-visited-caves next-small-twice-used)))))))

(defun main ()
  (let ((graph (make-hash-table :test #'equal)))
    (with-open-file (f "input.txt" :direction :input)
      (loop for line = (read-line f nil)
            while line do
            (let* ((parts (split-string (string-trim '(#\Newline #\Return) line) #\-))
                   (a (first parts))
                   (b (second parts)))
              (unless (string= a "end")
                (unless (string= b "start")
                  (push b (gethash a graph nil))))
              (unless (string= b "end")
                (unless (string= a "start")
                  (push a (gethash b graph nil)))))))
    (format t "~a~%" (find-paths graph "start" "end" nil nil))
    (format t "~a~%" (find-paths graph "start" "end" nil t))))

(main)
