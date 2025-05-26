
(defconstant SIZE 119315717514047)
(defconstant ITER 101741582076661)

(defun mod-pow (base exp m)
  (loop with result = 1
        with b = (mod base m)
        for e = exp then (ash e -1)
        while (> e 0)
        do (when (oddp e)
             (setf result (mod (* result b) m)))
           (setf b (mod (* b b) m))
        finally (return result)))

(defun mod-inv (a m)
  (mod-pow a (- m 2) m))

(defun main ()
  (let ((increment 1)
        (offset 0))
    (with-open-file (stream "input.txt" :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            do (cond
                 ((string= line "deal into new stack")
                  (setf increment (mod (- increment) SIZE))
                  (setf offset (mod (+ offset increment) SIZE)))
                 ((string= (subseq line 0 3) "cut")
                  (let ((n (parse-integer (subseq line 4))))
                    (setf offset (mod (+ offset (* n increment)) SIZE))))
                 ((string= (subseq line 0 19) "deal with increment")
                  (let* ((n (parse-integer (subseq line 20)))
                         (inv-n (mod-inv n SIZE)))
                    (setf increment (mod (* increment inv-n) SIZE)))))))
    (let* ((final-increment (mod-pow increment ITER SIZE))
           (inv-inc-minus-1 (mod-inv (mod (- increment 1) SIZE) SIZE))
           (final-offset (mod (* offset
                                  (mod (- final-increment 1) SIZE)
                                  inv-inc-minus-1)
                              SIZE))
           (answer (mod (+ (* 2020 final-increment) final-offset) SIZE)))
      (princ answer))))

(main)
