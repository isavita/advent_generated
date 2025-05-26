
(defconstant +MODULUS+ 20201227)
(defconstant +SUBJECT-NUMBER+ 7)

(defun transform-subject-number (base exp)
  (let ((res 1))
    (loop
      (when (zerop exp) (return res))
      (when (logbitp 0 exp)
        (setf res (mod (* res base) +MODULUS+)))
      (setf base (mod (* base base) +MODULUS+))
      (setf exp (ash exp -1)))))

(defun find-loop-size (public-key)
  (loop :with value = 1
        :for loop-size :from 0
        :do (when (= value public-key) (return loop-size))
            (setf value (mod (* value +SUBJECT-NUMBER+) +MODULUS+))))

(defun main ()
  (let (card-public-key door-public-key)
    (with-open-file (stream "input.txt" :direction :input)
      (setf card-public-key (parse-integer (read-line stream)))
      (setf door-public-key (parse-integer (read-line stream))))
    (let ((card-loop-size (find-loop-size card-public-key)))
      (print (transform-subject-number door-public-key card-loop-size)))))

(main)
