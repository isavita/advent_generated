
(defun read-file-lines (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun parse-input (lines)
  (let ((initial-state "")
        (rules (make-hash-table :test 'equal)))
    (loop for line in lines
          do (let ((initial-state-pos (search "initial state: " line)))
               (cond
                 (initial-state-pos
                  (setf initial-state (subseq line (+ initial-state-pos (length "initial state: "))))
                  )
                 ((search "=>" line)
                  (let* ((arrow-pos (search " => " line))
                         (pattern (string-trim '(#\Space) (subseq line 0 arrow-pos)))
                         (result (string-trim '(#\Space) (subseq line (+ arrow-pos (length " => ")))))
                         )
                    (setf (gethash pattern rules) (char= (char result 0) #\#)))))))
    (values initial-state rules)))

(defun get-min-max-keys (state)
  (let ((min-k nil) (max-k nil))
    (maphash (lambda (k v)
               (declare (ignore v))
               (if (null min-k)
                   (setf min-k k max-k k)
                   (progn
                     (when (< k min-k) (setf min-k k))
                     (when (> k max-k) (setf max-k k)))))
             state)
    (values min-k max-k)))

(defun get-state-pattern-and-sum (state)
  (multiple-value-bind (min-pot max-pot) (get-min-max-keys state)
    (if (null min-pot)
        (values "" 0)
        (let ((pattern-chars (make-array (+ (- max-pot min-pot) 1) :element-type 'character))
              (current-sum 0))
          (loop for i from min-pot to max-pot
                for idx from 0
                do (if (gethash i state)
                       (progn
                         (setf (aref pattern-chars idx) #\#)
                         (incf current-sum i))
                       (setf (aref pattern-chars idx) #\.)))
          (values (coerce pattern-chars 'string) current-sum)))))

(defun main ()
  (multiple-value-bind (initial-state rules) (parse-input (read-file-lines "input.txt"))
    (let ((state (make-hash-table :test 'eql))
          (previous-pattern "")
          (previous-sum 0))

      (loop for i from 0 below (length initial-state)
            do (when (char= (char initial-state i) #\#)
                 (setf (gethash i state) t)))

      (loop for generation from 0 below 50000000000
            do (let ((new-state (make-hash-table :test 'eql)))
                 (multiple-value-bind (min-pot max-pot) (get-min-max-keys state)
                   (when min-pot
                     (loop for i from (- min-pot 2) to (+ max-pot 2)
                           do (let* ((pattern-chars (make-array 5 :element-type 'character)))
                                (loop for j from (- i 2) to (+ i 2)
                                      for idx from 0
                                      do (setf (aref pattern-chars idx) (if (gethash j state) #\# #\.)))
                                (when (gethash (coerce pattern-chars 'string) rules)
                                  (setf (gethash i new-state) t))))))
                 (setf state new-state)

                 (multiple-value-bind (current-pattern current-sum) (get-state-pattern-and-sum state)
                   (when (string= current-pattern previous-pattern)
                     (let* ((offset (- current-sum previous-sum))
                            (remaining-generations (- 50000000000 generation 1))
                            (final-sum (+ current-sum (* offset remaining-generations))))
                       (format t "~a~%" final-sum)
                       (return)))
                   (setf previous-pattern current-pattern)
                   (setf previous-sum current-sum)))))))

(main)
