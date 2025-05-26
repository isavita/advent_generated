
(defun simple-split (s &optional (delimiter #\Space))
  (loop for i = 0 then (1+ j)
        as j = (position delimiter s :start i)
        collect (subseq s i (or j (length s)))
        while j))

(defun parse-line (line)
  (let* ((trimmed-line (string-trim '(#\Space #\Tab #\Newline #\Return) line))
         (parts (remove-if (lambda (s) (string= s "")) (simple-split trimmed-line))))
    (let ((opcode (car parts))
          (args (cdr parts)))
      (cons (intern (string-upcase opcode) "KEYWORD")
            (mapcar (lambda (part)
                      (let ((num (ignore-errors (parse-integer part))))
                        (if (and num (string= part (princ-to-string num)))
                            num
                            part)))
                    args)))))

(defun get-value (arg registers)
  (typecase arg
    (integer arg)
    (string (gethash arg registers))))

(defun run-code (initial-a instructions)
  (let ((registers (make-hash-table :test 'equal))
        (clock-signal '())
        (idx 0))

    (setf (gethash "a" registers) initial-a
          (gethash "b" registers) 0
          (gethash "c" registers) 0
          (gethash "d" registers) 0)

    (tagbody
       loop-start
       (when (>= idx (length instructions))
         (return-from run-code nil))

       (let* ((instr (nth idx instructions))
              (opcode (car instr))
              (arg1 (second instr))
              (arg2 (third instr)))

         (case opcode
           (:CPY
            (let ((val (get-value arg1 registers)))
              (setf (gethash arg2 registers) val)))
           (:INC
            (incf (gethash arg1 registers)))
           (:DEC
            (decf (gethash arg1 registers)))
           (:JNZ
            (let ((val (get-value arg1 registers)))
              (unless (zerop val)
                (incf idx (get-value arg2 registers))
                (go loop-start))))
           (:OUT
            (let ((val (get-value arg1 registers)))
              (when (and clock-signal (= val (car clock-signal)))
                (return-from run-code nil))
              (push val clock-signal)
              (when (= (length clock-signal) 10)
                (return-from run-code t))))))

       (incf idx)
       (go loop-start))))

(defun main ()
  (let ((instructions (with-open-file (f "input.txt")
                        (loop for line = (read-line f nil nil)
                              while line
                              collect (parse-line line)))))
    (loop for a from 0
          when (run-code a instructions)
            do (progn (format t "~a~%" a)
                      (return)))))

(main)
