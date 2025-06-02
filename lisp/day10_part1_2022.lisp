
(defvar *total-sum* 0)
(defvar *current-x* 1)
(defvar *cycle* 1)

(defun check-and-add-signal ()
  (when (= (mod *cycle* 40) 20)
    (setf *total-sum* (+ *total-sum* (* *cycle* *current-x*)))))

(defun main ()
  (setf *total-sum* 0)
  (setf *current-x* 1)
  (setf *cycle* 1)
  (with-open-file (stream "input.txt" :direction :input :if-does-not-exist :error)
    (loop for line = (read-line stream nil nil)
          while line
          do
          (cond
            ((string= line "noop")
             (check-and-add-signal)
             (incf *cycle*))
            (t
             (let* ((space-pos (position #\Space line))
                    (value (parse-integer (subseq line (1+ space-pos)))))
               (check-and-add-signal)
               (incf *cycle*)
               (check-and-add-signal)
               (incf *cycle*)
               (incf *current-x* value))))))
  (format t "~a~%" *total-sum*))

(main)
