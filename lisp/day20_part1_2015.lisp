
#+quicklisp
(ql:quickload :uiop :silent t)

(defun find-lowest-house-number (target-presents)
  (declare (type (integer 0 *) target-presents)
           (optimize (speed 3) (safety 0) (debug 0)))

  (let ((houses (make-array target-presents :element-type '(unsigned-byte 32) :initial-element 0)))
    (declare (type (simple-array (unsigned-byte 32) (*)) houses))

    (loop for elf from 1 to (floor target-presents 10)
          do
             (loop for house from elf below target-presents by elf
                   do
                     (incf (aref houses house) (* elf 10))))

    (loop for house from 1 below target-presents
          do
             (when (>= (aref houses house) target-presents)
               (return-from find-lowest-house-number house)))
    -1))

(defun main ()
  (let ((input-file "input.txt"))
    (handler-case
        (with-open-file (in input-file :direction :input
                                        :if-does-not-exist :error)
          (let* ((line (read-line in nil))
                 (target-presents (if line
                                      (parse-integer (string-trim '(#\Space #\Newline #\Return) line) :junk-allowed t)
                                      nil)))
            (if (and (numberp target-presents) (not (minusp target-presents)))
                (format t "~A~%" (find-lowest-house-number target-presents))
                (format *error-output* "Invalid input. Please provide a valid positive number.~%"))))
      (error (c)
        (format *error-output* "Error: ~A~%" c)))))

(main)
