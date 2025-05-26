
(defun count-char-frequencies (s)
  (let ((counts (make-hash-table)))
    (loop for char across s do
      (incf (gethash char counts 0)))
    counts))

(defun check-counts (counts-table)
  (let ((has-two nil)
        (has-three nil))
    (loop for count being the hash-values of counts-table do
      (when (= count 2) (setf has-two t))
      (when (= count 3) (setf has-three t))
      (when (and has-two has-three) (return)))
    (values has-two has-three)))

(defun main ()
  (let ((num-twos 0)
        (num-threes 0))
    (with-open-file (file "input.txt" :direction :input)
      (loop for line = (read-line file nil nil)
            while line do
              (multiple-value-bind (has-two has-three)
                  (check-counts (count-char-frequencies line))
                (when has-two (incf num-twos))
                (when has-three (incf num-threes)))))
    (print (* num-twos num-threes))))

(main)
