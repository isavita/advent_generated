
(defun split-string (string delimiter)
  (loop for i = 0 then (1+ j)
        for j = (position delimiter string :start i)
        collect (subseq string i (or j (length string)))
        while j))

(defun read-intcode-program (filename)
  (with-open-file (s filename)
    (coerce (mapcar #'parse-integer (split-string (read-line s) #\,)) 'vector)))

(defun run-intcode (noun verb initial-program)
  (let* ((p (copy-seq initial-program))
         (i 0))
    (setf (aref p 1) noun)
    (setf (aref p 2) verb)
    (loop
      (let ((op (aref p i)))
        (cond ((= op 99) (return))
              ((= op 1)
               (setf (aref p (aref p (+ i 3)))
                     (+ (aref p (aref p (+ i 1)))
                        (aref p (aref p (+ i 2))))))
              ((= op 2)
               (setf (aref p (aref p (+ i 3)))
                     (* (aref p (aref p (+ i 1)))
                        (aref p (aref p (+ i 2))))))
              (t (error "Unknown opcode ~A at ~A" op i))))
      (incf i 4))
    (aref p 0)))

(defun main ()
  (let ((initial-program (read-intcode-program "input.txt")))
    (format t "~A~%" (run-intcode 12 2 initial-program))
    (loop for n from 0 below 100 do
      (loop for v from 0 below 100 do
        (when (= (run-intcode n v initial-program) 19690720)
          (format t "~A~%" (+ (* 100 n) v))
          (return-from main))))))

(main)
