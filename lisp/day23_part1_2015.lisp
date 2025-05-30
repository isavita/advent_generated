
(defun simple-split (s char)
  (loop for i = 0 then (1+ j)
        for j = (position char s :start i)
        collect (subseq s i (or j (length s)))
        while j))

(defun split-on-string (string delimiter)
  (let ((parts '())
        (start 0)
        (delim-len (length delimiter)))
    (loop
      (let ((pos (search delimiter string :start2 start)))
        (if pos
            (progn
              (push (subseq string start pos) parts)
              (setf start (+ pos delim-len)))
            (progn
              (push (subseq string start (length string)) parts)
              (return (nreverse parts))))))))

(defun parse-instruction (line)
  (let* ((parts-comma (split-on-string line ", "))
         (first-part-raw (car parts-comma))
         (first-part-words (simple-split first-part-raw #\Space))
         (type-str (car first-part-words))
         (type (intern (string-upcase type-str) :keyword))
         (arg1-str (cadr first-part-words))
         (offset-str-raw (cadr parts-comma))
         (offset-val (when offset-str-raw (parse-integer offset-str-raw))))
    (case type
      (:jmp `(,type nil ,(parse-integer arg1-str)))
      ((:jie :jio) `(,type ,(intern (string-upcase arg1-str) :keyword) ,offset-val))
      (otherwise `(,type ,(intern (string-upcase arg1-str) :keyword) nil)))))

(defun read-instructions (filepath)
  (with-open-file (stream filepath :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect (parse-instruction line))))

(defun execute-program (instructions)
  (let ((registers (make-hash-table :test 'eq))
        (pointer 0))
    (setf (gethash :a registers) 0)
    (setf (gethash :b registers) 0)
    (loop
      (when (or (< pointer 0) (>= pointer (length instructions)))
        (return (gethash :b registers)))
      (let* ((instr (nth pointer instructions))
             (type (car instr))
             (reg (cadr instr))
             (offset (caddr instr)))
        (case type
          (:hlf (setf (gethash reg registers) (floor (gethash reg registers) 2))
                (incf pointer))
          (:tpl (setf (gethash reg registers) (* (gethash reg registers) 3))
                (incf pointer))
          (:inc (incf (gethash reg registers))
                (incf pointer))
          (:jmp (incf pointer offset))
          (:jie (if (evenp (gethash reg registers))
                    (incf pointer offset)
                    (incf pointer)))
          (:jio (if (= 1 (gethash reg registers))
                    (incf pointer offset)
                    (incf pointer)))
          (otherwise (error "Unknown instruction type: ~a" type)))))))

(defun main ()
  (handler-case
      (let* ((instructions (read-instructions "input.txt"))
             (result (execute-program instructions)))
        (format t "~a~%" result))
    (error (e)
      (format *error-output* "Error: ~a~%" e))))

(main)
