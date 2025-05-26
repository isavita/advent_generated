
(defun read-containers (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil nil)
          while line
          collect (parse-integer line))))

(defun count-combinations-recursive (items target)
  (cond
    ((zerop target) 1)
    ((null items) 0)
    ((< target 0) 0)
    ((< target (car items))
     (count-combinations-recursive (cdr items) target))
    (t
     (+ (count-combinations-recursive (cdr items) (- target (car items)))
        (count-combinations-recursive (cdr items) target)))))

(defun main ()
  (let* ((containers (read-containers "input.txt"))
         (target 150))
    (format t "~a~%" (count-combinations-recursive containers target))))

(main)
