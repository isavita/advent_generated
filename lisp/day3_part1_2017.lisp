
(defun find-steps (num)
  (if (= num 1)
      0
      (let* ((root (ceiling (sqrt num)))
             (adjusted-root (if (evenp root) (1+ root) root))
             (side-length (- adjusted-root 1))
             (steps-to-center (floor side-length 2))
             (max-num (* adjusted-root adjusted-root))
             (steps-to-max-num (- max-num num))
             (steps-along-side (mod steps-to-max-num side-length)))
        (+ steps-to-center (abs (- steps-along-side steps-to-center))))))

(defun main ()
  (with-open-file (stream "input.txt" :direction :input)
    (let ((num (read stream)))
      (print (find-steps num)))))

(main)
