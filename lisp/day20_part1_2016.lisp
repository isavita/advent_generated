
(defun split-string (s char)
  (loop with result = '()
        with start = 0
        for i from 0 to (length s)
        do (if (or (= i (length s)) (char= (char s i) char))
               (progn
                 (push (subseq s start i) result)
                 (setf start (1+ i))))
        finally (return (nreverse result))))

(defun main ()
  (let ((blocked-ips '()))
    (with-open-file (file "input.txt" :direction :input)
      (loop for line = (read-line file nil nil)
            while line
            do (let* ((parts (split-string line #\-))
                      (start (parse-integer (first parts)))
                      (end (parse-integer (second parts))))
                 (push (list start end) blocked-ips))))

    (setf blocked-ips (sort blocked-ips (lambda (a b)
                                          (or (< (first a) (first b))
                                              (and (= (first a) (first b))
                                                   (< (second a) (second b)))))))

    (let ((current-ip 0))
      (loop for range in blocked-ips
            do (let ((start (first range))
                     (end (second range)))
                 (when (< current-ip start)
                   (format t "~a~%" current-ip)
                   (return))
                 (setf current-ip (max current-ip (+ end 1)))))
      (format t "~a~%" current-ip))))

(main)
