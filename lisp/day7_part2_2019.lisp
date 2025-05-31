
(defstruct intcode-computer
  (memory #() :type (simple-array fixnum (*)))
  (pointer 0 :type fixnum)
  (inputs nil :type list)
  (outputs nil :type list)
  (halted nil :type boolean))

(declaim (inline get-param))
(defun get-param (computer mode value)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type intcode-computer computer)
           (type fixnum mode value))
  (if (= mode 1)
      value
      (aref (intcode-computer-memory computer) value)))

(defun run-intcode-computer (computer)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type intcode-computer computer))
  (loop
    (let* ((mem (intcode-computer-memory computer))
           (ptr (intcode-computer-pointer computer))
           (instruction (aref mem ptr))
           (opcode (the fixnum (mod instruction 100)))
           (modes (the fixnum (floor instruction 100)))
           (mode1 (the fixnum (mod modes 10)))
           (mode2 (the fixnum (mod (the fixnum (floor modes 10)) 10)))
           (mode3 (the fixnum (mod (the fixnum (floor modes 100)) 10))))

      (case opcode
        (1
         (let* ((p1 (get-param computer mode1 (aref mem (+ ptr 1))))
                (p2 (get-param computer mode2 (aref mem (+ ptr 2))))
                (dest (aref mem (+ ptr 3))))
           (setf (aref mem dest) (+ p1 p2))
           (incf (intcode-computer-pointer computer) 4)))
        (2
         (let* ((p1 (get-param computer mode1 (aref mem (+ ptr 1))))
                (p2 (get-param computer mode2 (aref mem (+ ptr 2))))
                (dest (aref mem (+ ptr 3))))
           (setf (aref mem dest) (* p1 p2))
           (incf (intcode-computer-pointer computer) 4)))
        (3
         (when (null (intcode-computer-inputs computer))
           (return-from run-intcode-computer :needs-input))
         (let* ((dest (aref mem (+ ptr 1)))
                (input-val (pop (intcode-computer-inputs computer))))
           (setf (aref mem dest) input-val)
           (incf (intcode-computer-pointer computer) 2)))
        (4
         (let* ((p1 (get-param computer mode1 (aref mem (+ ptr 1)))))
           (push p1 (intcode-computer-outputs computer))
           (incf (intcode-computer-pointer computer) 2)
           (return-from run-intcode-computer p1)))
        (5
         (let* ((p1 (get-param computer mode1 (aref mem (+ ptr 1))))
                (p2 (get-param computer mode2 (aref mem (+ ptr 2)))))
           (if (/= p1 0)
               (setf (intcode-computer-pointer computer) p2)
               (incf (intcode-computer-pointer computer) 3))))
        (6
         (let* ((p1 (get-param computer mode1 (aref mem (+ ptr 1))))
                (p2 (get-param computer mode2 (aref mem (+ ptr 2)))))
           (if (= p1 0)
               (setf (intcode-computer-pointer computer) p2)
               (incf (intcode-computer-pointer computer) 3))))
        (7
         (let* ((p1 (get-param computer mode1 (aref mem (+ ptr 1))))
                (p2 (get-param computer mode2 (aref mem (+ ptr 2))))
                (dest (aref mem (+ ptr 3))))
           (setf (aref mem dest) (if (< p1 p2) 1 0))
           (incf (intcode-computer-pointer computer) 4)))
        (8
         (let* ((p1 (get-param computer mode1 (aref mem (+ ptr 1))))
                (p2 (get-param computer mode2 (aref mem (+ ptr 2))))
                (dest (aref mem (+ ptr 3))))
           (setf (aref mem dest) (if (= p1 p2) 1 0))
           (incf (intcode-computer-pointer computer) 4)))
        (99
         (setf (intcode-computer-halted computer) t)
         (return-from run-intcode-computer :halted))
        (t
         (error "Unknown opcode ~a at position ~a" opcode ptr))))
    (when (>= (intcode-computer-pointer computer) (length (intcode-computer-memory computer)))
      (setf (intcode-computer-halted computer) t)
      (return-from run-intcode-computer :halted))))

(defun make-computer-from-program (program initial-inputs)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type list program initial-inputs))
  (make-intcode-computer
   :memory (make-array (length program)
                       :element-type 'fixnum
                       :initial-contents program)
   :inputs initial-inputs))

(defun generate-permutations (lst)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type list lst))
  (if (null (cdr lst))
      (list lst)
      (loop for x in lst
            append (mapcar (lambda (p) (cons x p))
                           (generate-permutations (remove x lst :count 1))))))

(defun parse-int-list (s separator)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type string s)
           (type character separator))
  (let ((start 0)
        (end (length s))
        (result '()))
    (loop
      (let ((comma-pos (position separator s :start start)))
        (if comma-pos
            (progn
              (push (parse-integer s :start start :end comma-pos) result)
              (setf start (1+ comma-pos)))
            (progn
              (push (parse-integer s :start start :end end) result)
              (return (nreverse result))))))))

(defun part-one (initial-program)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type list initial-program))
  (let ((phase-settings '(0 1 2 3 4))
        (max-signal 0))
    (dolist (phase-seq (generate-permutations phase-settings))
      (let ((input 0))
        (dolist (phase phase-seq)
          (let* ((amp (make-computer-from-program initial-program (list phase input)))
                 (output (run-intcode-computer amp)))
            (setf input output)))
        (setf max-signal (max max-signal input))))
    max-signal))

(defun part-two (initial-program)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type list initial-program))
  (let ((phase-settings '(5 6 7 8 9))
        (max-signal 0))
    (dolist (phase-seq (generate-permutations phase-settings))
      (let* ((amplifiers (loop for phase in phase-seq
                               collect (make-computer-from-program initial-program (list phase))))
             (input 0)
             (last-output 0)
             (current-amp 0))
        (setf (intcode-computer-inputs (first amplifiers)) (append (intcode-computer-inputs (first amplifiers)) (list input)))

        (loop
          (let* ((amp (nth current-amp amplifiers)))
            (let ((output (run-intcode-computer amp)))
              (cond
                ((eq output :halted)
                 (when (every #'intcode-computer-halted amplifiers)
                   (return)))
                ((eq output :needs-input)
                 nil)
                (t
                 (setf input output)
                 (when (= current-amp (- (length amplifiers) 1))
                   (setf last-output input))
                 (let ((next-amp (nth (mod (1+ current-amp) (length amplifiers)) amplifiers)))
                   (setf (intcode-computer-inputs next-amp) (append (intcode-computer-inputs next-amp) (list input)))))))
          (setf current-amp (mod (1+ current-amp) (length amplifiers)))))
        (setf max-signal (max max-signal last-output))))
    max-signal))

(defun main ()
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((program (with-open-file (f "input.txt" :direction :input)
                   (let ((line (read-line f nil nil)))
                     (unless line (error "Input file 'input.txt' is empty or not found."))
                     (parse-int-list line #\,))))
        (p1 nil)
        (p2 nil))

    (setf p1 (part-one program))
    (setf p2 (part-two program))

    (format t "~a~%" p1)
    (format t "~a~%" p2)))

(main)
