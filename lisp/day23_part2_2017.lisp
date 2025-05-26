
(defun is-prime (n)
  (cond
    ((< n 2) nil)
    ((= n 2) t)
    ((evenp n) nil)
    (t (loop for i from 3 to (isqrt n) by 2
             when (zerop (mod n i))
             do (return-from is-prime nil))
       t)))

(defun main ()
  (let* ((b (+ (* 57 100) 100000))
         (c (+ b 17000))
         (h 0))
    (loop for x from b to c by 17
          when (not (is-prime x))
          do (incf h))
    (format t "~a~%" h)))

(main)
