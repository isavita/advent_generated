
(defun split-string (string delimiter)
  (loop for start = 0 then (1+ end)
        for end = (position delimiter string :start start)
        collect (subseq string start end)
        while end))

(defun copy-hash-table (original-table)
  (let ((new-table (make-hash-table :test (hash-table-test original-table)
                                    :size (hash-table-size original-table))))
    (maphash (lambda (key value)
               (setf (gethash key new-table) value))
             original-table)
    new-table))

(defstruct intcode-computer
  (memory (make-hash-table) :type hash-table)
  (pointer 0 :type (and fixnum unsigned-byte))
  (relative-base 0 :type (signed-byte 64)))

(defun get-val (mem addr)
  (the integer (gethash addr mem 0)))

(defun set-val (mem addr val)
  (setf (gethash addr mem) (the integer val)))

(defun get-param (computer mode offset)
  (let* ((mem (intcode-computer-memory computer))
         (ptr (intcode-computer-pointer computer))
         (rel-base (intcode-computer-relative-base computer))
         (param (get-val mem (+ ptr offset))))
    (case mode
      (0 (get-val mem param))
      (1 param)
      (2 (get-val mem (+ rel-base param)))
      (t (error "Unknown mode: ~a" mode)))))

(defun set-param (computer mode offset value)
  (let* ((mem (intcode-computer-memory computer))
         (ptr (intcode-computer-pointer computer))
         (rel-base (intcode-computer-relative-base computer))
         (param (get-val mem (+ ptr offset))))
    (case mode
      (0 (set-val mem param value))
      (2 (set-val mem (+ rel-base param) value))
      (t (error "Unknown mode for writing: ~a" mode)))))

(defun run-intcode (computer input)
  (loop
    (let* ((mem (intcode-computer-memory computer))
           (ptr (intcode-computer-pointer computer))
           (instruction (get-val mem ptr))
           (opcode (mod instruction 100))
           (mode1 (mod (floor instruction 100) 10))
           (mode2 (mod (floor instruction 1000) 10))
           (mode3 (mod (floor instruction 10000) 10)))

      (case opcode
        (1 (set-param computer mode3 3 (+ (get-param computer mode1 1) (get-param computer mode2 2)))
           (incf (intcode-computer-pointer computer) 4))
        (2 (set-param computer mode3 3 (* (get-param computer mode1 1) (get-param computer mode2 2)))
           (incf (intcode-computer-pointer computer) 4))
        (3 (set-param computer mode1 1 input)
           (incf (intcode-computer-pointer computer) 2))
        (4 (let ((output-val (get-param computer mode1 1)))
             (incf (intcode-computer-pointer computer) 2)
             (return output-val)))
        (5 (if (/= (get-param computer mode1 1) 0)
               (setf (intcode-computer-pointer computer) (get-param computer mode2 2))
               (incf (intcode-computer-pointer computer) 3)))
        (6 (if (= (get-param computer mode1 1) 0)
               (setf (intcode-computer-pointer computer) (get-param computer mode2 2))
               (incf (intcode-computer-pointer computer) 3)))
        (7 (set-param computer mode3 3 (if (< (get-param computer mode1 1) (get-param computer mode2 2)) 1 0))
           (incf (intcode-computer-pointer computer) 4))
        (8 (set-param computer mode3 3 (if (= (get-param computer mode1 1) (get-param computer mode2 2)) 1 0))
           (incf (intcode-computer-pointer computer) 4))
        (9 (incf (intcode-computer-relative-base computer) (get-param computer mode1 1))
           (incf (intcode-computer-pointer computer) 2))
        (99 (return -1))
        (t (error "Unknown opcode: ~a" opcode))))))

(defun read-program (filepath)
  (with-open-file (stream filepath :direction :input)
    (let* ((line (read-line stream nil ""))
           (parts (split-string line #\,))
           (program (make-hash-table)))
      (loop for part in parts
            for i from 0
            do (setf (gethash i program) (parse-integer part)))
      program)))

(defun main ()
  (let* ((initial-memory (read-program "input.txt"))
         (computer (make-intcode-computer :memory (copy-hash-table initial-memory))))
    (print (run-intcode computer 2))))

(main)
