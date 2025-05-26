
(defun main ()
  (let ((accumulator 0)
        (index 0)
        (visited (make-hash-table))
        (instructions (make-array 0 :adjustable t :fill-pointer 0)))

    (with-open-file (file "input.txt" :direction :input)
      (loop for line = (read-line file nil nil)
            while line
            do (vector-push-extend (string-trim '(#\Space #\Tab #\Newline #\Return) line) instructions)))

    (loop
      (when (gethash index visited)
        (return))
      (setf (gethash index visited) t)

      (let* ((instruction-line (aref instructions index))
             (space-pos (position #\Space instruction-line))
             (instruction (subseq instruction-line 0 space-pos))
             (argument (parse-integer (subseq instruction-line (1+ space-pos)))))

        (cond ((string= instruction "acc")
               (incf accumulator argument)
               (incf index))
              ((string= instruction "jmp")
               (incf index argument))
              ((string= instruction "nop")
               (incf index)))))
    (format t "~a~%" accumulator)))

(main)
