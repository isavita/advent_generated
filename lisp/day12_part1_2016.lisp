
(defvar *registers* (make-hash-table :test 'eq))
(defvar *instructions* (make-array 0 :adjustable t :fill-pointer 0))

(defun split-string (s delimiter)
  (loop for start = 0 then (1+ end)
        for end = (position delimiter s :start start)
        collect (subseq s start (or end (length s)))
        while end))

(defun parse-arg (s)
  (let ((num (parse-integer s :junk-allowed t)))
    (if num
        num
        (intern (string-upcase s)))))

(defun get-value (arg)
  (if (numberp arg)
      arg
      (gethash arg *registers*)))

(defun main ()
  (setf (gethash 'a *registers*) 0
        (gethash 'b *registers*) 0
        (gethash 'c *registers*) 0
        (gethash 'd *registers*) 0)

  (with-open-file (f "input.txt" :direction :input)
    (loop for line = (read-line f nil)
          while line
          do (let ((parts (split-string line #\Space)))
               (vector-push-extend
                (cons (intern (string-upcase (car parts)))
                      (mapcar #'parse-arg (cdr parts)))
                *instructions*))))

  (loop with i = 0
        while (< i (length *instructions*))
        do (let* ((instruction (aref *instructions* i))
                  (op (car instruction))
                  (args (cdr instruction))
                  (next-i (1+ i)))
             (cond
               ((eq op 'cpy)
                (setf (gethash (second args) *registers*)
                      (get-value (first args))))
               ((eq op 'inc)
                (incf (gethash (first args) *registers*)))
               ((eq op 'dec)
                (decf (gethash (first args) *registers*)))
               ((eq op 'jnz)
                (when (not (zerop (get-value (first args))))
                  (setf next-i (+ i (second args)))))
               (t (error "Unknown instruction: ~a" op)))
             (setf i next-i)))

  (princ (gethash 'a *registers*)))

(main)
