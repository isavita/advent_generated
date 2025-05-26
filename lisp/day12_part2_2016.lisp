
(defun split-string (string delimiter)
  (loop for i = 0 then (1+ j)
        for j = (position delimiter string :start i)
        collect (subseq string i (or j (length string)))
        while j))

(defun parse-operand (str)
  (let ((val (ignore-errors (parse-integer str))))
    (if val
        val
        (intern (string-upcase str)))))

(defun parse-instruction (line)
  (let* ((parts (split-string line #\Space))
         (opcode (intern (string-upcase (first parts)))))
    (case opcode
      ('CPY (list opcode (parse-operand (second parts)) (intern (string-upcase (third parts)))))
      ('INC (list opcode (intern (string-upcase (second parts)))))
      ('DEC (list opcode (intern (string-upcase (second parts)))))
      ('JNZ (list opcode (parse-operand (second parts)) (parse-integer (third parts))))
      (t (error "Unknown opcode: ~a" opcode)))))

(defun read-instructions (filename)
  (with-open-file (file filename :direction :input)
    (coerce (loop for line = (read-line file nil nil)
                  while line
                  collect (parse-instruction line))
            'vector)))

(defun get-value (operand registers)
  (if (integerp operand)
      operand
      (gethash operand registers)))

(defun execute-instructions (instructions registers)
  (let ((ip 0)
        (num-instructions (length instructions)))
    (loop while (< ip num-instructions)
          do (let* ((instr (aref instructions ip))
                    (opcode (first instr)))
               (case opcode
                 ('CPY (let ((val (get-value (second instr) registers))
                              (dest-reg (third instr)))
                         (setf (gethash dest-reg registers) val)
                         (incf ip)))
                 ('INC (let ((reg (second instr)))
                         (incf (gethash reg registers))
                         (incf ip)))
                 ('DEC (let ((reg (second instr)))
                         (decf (gethash reg registers))
                         (incf ip)))
                 ('JNZ (let ((val (get-value (second instr) registers))
                              (jump (third instr)))
                         (if (/= val 0)
                             (incf ip jump)
                             (incf ip))))
                 (t (error "Invalid instruction: ~a at ip ~a" instr ip)))))))

(defun main ()
  (let ((registers (make-hash-table :test 'eq)))
    (setf (gethash 'a registers) 0)
    (setf (gethash 'b registers) 0)
    (setf (gethash 'c registers) 1)
    (setf (gethash 'd registers) 0)

    (let ((instructions (read-instructions "input.txt")))
      (execute-instructions instructions registers)
      (format t "~a~%" (gethash 'a registers)))))

(main)
