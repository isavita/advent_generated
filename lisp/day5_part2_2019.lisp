
(defun get-parameter (program mode value)
  (if (= mode 0)
      (aref program value)
      value))

(defun split-string (string delimiter)
  (loop for i = 0 then (1+ j)
        for j = (position delimiter string :start i)
        collect (subseq string i (or j (length string)))
        while j))

(defun run-intcode (program input-value)
  (let ((pointer 0)
        (program-copy (copy-seq program))
        (last-output nil))
    (loop
      (let* ((instruction (aref program-copy pointer))
             (opcode (mod instruction 100))
             (mode1 (mod (floor instruction 100) 10))
             (mode2 (mod (floor instruction 1000) 10)))
        (case opcode
          (1
           (let* ((p1-addr (aref program-copy (+ pointer 1)))
                  (p2-addr (aref program-copy (+ pointer 2)))
                  (dest-addr (aref program-copy (+ pointer 3)))
                  (val1 (get-parameter program-copy mode1 p1-addr))
                  (val2 (get-parameter program-copy mode2 p2-addr)))
             (setf (aref program-copy dest-addr) (+ val1 val2))
             (incf pointer 4)))
          (2
           (let* ((p1-addr (aref program-copy (+ pointer 1)))
                  (p2-addr (aref program-copy (+ pointer 2)))
                  (dest-addr (aref program-copy (+ pointer 3)))
                  (val1 (get-parameter program-copy mode1 p1-addr))
                  (val2 (get-parameter program-copy mode2 p2-addr)))
             (setf (aref program-copy dest-addr) (* val1 val2))
             (incf pointer 4)))
          (3
           (let ((dest-addr (aref program-copy (+ pointer 1))))
             (setf (aref program-copy dest-addr) input-value)
             (incf pointer 2)))
          (4
           (let* ((p1-addr (aref program-copy (+ pointer 1)))
                  (val1 (get-parameter program-copy mode1 p1-addr)))
             (setf last-output val1)
             (incf pointer 2)))
          (5
           (let* ((p1-addr (aref program-copy (+ pointer 1)))
                  (p2-addr (aref program-copy (+ pointer 2)))
                  (val1 (get-parameter program-copy mode1 p1-addr))
                  (val2 (get-parameter program-copy mode2 p2-addr)))
             (setf pointer (if (/= val1 0) val2 (+ pointer 3)))))
          (6
           (let* ((p1-addr (aref program-copy (+ pointer 1)))
                  (p2-addr (aref program-copy (+ pointer 2)))
                  (val1 (get-parameter program-copy mode1 p1-addr))
                  (val2 (get-parameter program-copy mode2 p2-addr)))
             (setf pointer (if (= val1 0) val2 (+ pointer 3)))))
          (7
           (let* ((p1-addr (aref program-copy (+ pointer 1)))
                  (p2-addr (aref program-copy (+ pointer 2)))
                  (dest-addr (aref program-copy (+ pointer 3)))
                  (val1 (get-parameter program-copy mode1 p1-addr))
                  (val2 (get-parameter program-copy mode2 p2-addr)))
             (setf (aref program-copy dest-addr) (if (< val1 val2) 1 0))
             (incf pointer 4)))
          (8
           (let* ((p1-addr (aref program-copy (+ pointer 1)))
                  (p2-addr (aref program-copy (+ pointer 2)))
                  (dest-addr (aref program-copy (+ pointer 3)))
                  (val1 (get-parameter program-copy mode1 p1-addr))
                  (val2 (get-parameter program-copy mode2 p2-addr)))
             (setf (aref program-copy dest-addr) (if (= val1 val2) 1 0))
             (incf pointer 4)))
          (99
           (return-from run-intcode last-output))
          (t
           (error "Unknown opcode: ~a at pointer ~a" opcode pointer)))))))

(defun read-program-from-file (filename)
  (with-open-file (stream filename :direction :input)
    (let* ((line (read-line stream nil nil))
           (str-numbers (when line (split-string line #\,))))
      (map 'vector #'parse-integer str-numbers))))

(defun main ()
  (let* ((program (read-program-from-file "input.txt"))
         (diagnostic-code (run-intcode program 5)))
    (format t "~a~%" diagnostic-code)))

(main)
