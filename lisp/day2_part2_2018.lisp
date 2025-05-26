
(defun count-char-frequencies (s)
  (let ((counts (make-hash-table :test 'eql)))
    (loop for c across s
          do (incf (gethash c counts 0)))
    counts))

(defun part-one (box-ids)
  (loop with count-two = 0
        with count-three = 0
        for box-id in box-ids
        do (let ((counts (count-char-frequencies box-id))
                  (has-two nil)
                  (has-three nil))
             (maphash (lambda (char count)
                        (declare (ignore char))
                        (when (= count 2) (setf has-two t))
                        (when (= count 3) (setf has-three t)))
                      counts)
             (when has-two (incf count-two))
             (when has-three (incf count-three)))
        finally (return (* count-two count-three))))

(defun compare-strings (s1 s2)
  (loop with diff-count = 0
        with common-chars-list = nil
        for c1 across s1
        for c2 across s2
        do (if (char= c1 c2)
               (push c1 common-chars-list)
               (incf diff-count))
        finally (return (values diff-count (coerce (nreverse common-chars-list) 'string)))))

(defun part-two (box-ids)
  (loop for s1-list on box-ids
        for s1 = (car s1-list)
        do (loop for s2-list on (cdr s1-list)
                 for s2 = (car s2-list)
                 do (multiple-value-bind (diff-count common-string) (compare-strings s1 s2)
                      (when (= diff-count 1)
                        (return-from part-two common-string))))))

(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect (string-trim '(#\Space #\Tab #\Newline #\Return) line))))

(defun main ()
  (let ((box-ids (read-input "input.txt")))
    (format t "~a~%" (part-one box-ids))
    (format t "~a~%" (part-two box-ids))))

(main)
