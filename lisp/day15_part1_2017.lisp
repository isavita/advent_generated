
(defconstant gen-a-factor 16807)
(defconstant gen-b-factor 48271)
(defconstant modulus 2147483647)
(defconstant iterations 40000000)
(defconstant mask #xFFFF)

(defun main ()
  (let (gen-a-start gen-b-start)
    (with-open-file (in "input.txt" :direction :input)
      (setf gen-a-start (parse-integer (read-line in)))
      (setf gen-b-start (parse-integer (read-line in))))

    (let ((gen-a gen-a-start)
          (gen-b gen-b-start)
          (matches 0))
      (declare (type fixnum gen-a gen-b matches))
      (loop repeat iterations do
        (setf gen-a (mod (* gen-a gen-a-factor) modulus))
        (setf gen-b (mod (* gen-b gen-b-factor) modulus))
        (when (= (logand gen-a mask) (logand gen-b mask))
          (incf matches)))
      (format t "~a~%" matches))))

(main)
