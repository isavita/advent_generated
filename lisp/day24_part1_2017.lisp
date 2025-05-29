
(defun split-string (string delimiter)
  (loop for start = 0 then (1+ end)
        for end = (position delimiter string :start start)
        collect (subseq string start (or end (length string)))
        while end))

(defun read-components (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect (destructuring-bind (p1-str p2-str)
                      (split-string line #\/)
                    (cons (parse-integer p1-str)
                          (parse-integer p2-str))) into comps-list
          finally (return (coerce comps-list 'vector)))))

(defun find-strongest-bridge (components current-port used-mask memo)
  (let ((key (cons current-port used-mask)))
    (multiple-value-bind (cached-value present-p)
        (gethash key memo)
      (if present-p
          cached-value
          (let ((max-strength 0))
            (loop for i from 0 below (length components)
                  for comp = (elt components i)
                  for p1 = (car comp)
                  for p2 = (cdr comp)
                  do (when (and (not (logbitp i used-mask))
                                (or (= p1 current-port)
                                    (= p2 current-port)))
                       (let* ((new-used-mask (logior used-mask (ash 1 i)))
                              (next-port (if (= p1 current-port) p2 p1))
                              (strength (+ p1 p2)))
                         (setf max-strength
                               (max max-strength
                                    (+ strength (find-strongest-bridge components next-port new-used-mask memo)))))))
            (setf (gethash key memo) max-strength))))))

(defun main ()
  (let* ((filename "input.txt")
         (components (read-components filename))
         (memo (make-hash-table :test 'equal)))
    (format t "~a~%"
            (find-strongest-bridge components 0 0 memo))))

(main)
