
(defun main ()
  (let* ((points (make-hash-table :test 'equal))
         (folds '())
         (reading-points-p t)
         (first-fold-count nil))

    (with-open-file (stream "input.txt" :direction :input)
      (loop for line = (read-line stream nil :eof)
            until (eq line :eof)
            do (let ((trimmed-line (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
                 (cond
                   ((string= trimmed-line "")
                    (setf reading-points-p nil))
                   (reading-points-p
                    (let* ((comma-pos (position #\, trimmed-line))
                           (x (parse-integer (subseq trimmed-line 0 comma-pos)))
                           (y (parse-integer (subseq trimmed-line (1+ comma-pos)))))
                      (setf (gethash (list x y) points) t)))
                   (t
                    (let* ((eq-pos (position #\= trimmed-line))
                           (axis (char trimmed-line (1- eq-pos)))
                           (value (parse-integer (subseq trimmed-line (1+ eq-pos)))))
                      (push (list axis value) folds)))))))

    (setf folds (nreverse folds))

    (loop for fold in folds
          for i from 0
          do (let ((new-points (make-hash-table :test 'equal)))
               (maphash (lambda (point val)
                          (declare (ignore val))
                          (let* ((x (first point))
                                 (y (second point))
                                 (axis (first fold))
                                 (fold-value (second fold)))
                            (cond
                              ((and (char= axis #\x) (> x fold-value))
                               (setf (gethash (list (- (* 2 fold-value) x) y) new-points) t))
                              ((and (char= axis #\y) (> y fold-value))
                               (setf (gethash (list x (- (* 2 fold-value) y)) new-points) t))
                              (t
                               (setf (gethash point new-points) t)))))
                        points)
               (setf points new-points)
               (when (= i 0)
                 (setf first-fold-count (hash-table-count points)))))

    (format t "Number of dots visible after first fold: ~a~%" first-fold-count)

    (let ((max-x 0)
          (max-y 0))
      (maphash (lambda (point val)
                 (declare (ignore val))
                 (setf max-x (max max-x (first point)))
                 (setf max-y (max max-y (second point))))
               points)

      (let ((grid (make-array (list (1+ max-y) (1+ max-x)) :initial-element #\.)))
        (maphash (lambda (point val)
                   (declare (ignore val))
                   (let ((x (first point))
                         (y (second point)))
                     (setf (aref grid y x) #\#)))
                 points)

        (loop for y from 0 to max-y
              do (loop for x from 0 to max-x
                       do (princ (aref grid y x)))
                 (terpri))))))

(main)
