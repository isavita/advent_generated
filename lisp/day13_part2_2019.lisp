
(defstruct intcode-computer
  (memory (make-hash-table :test 'eql))
  (ip 0)
  (relative-base 0)
  (halted nil))

(defun get-parameter (computer offset mode)
  (let* ((mem (intcode-computer-memory computer))
         (val (gethash (+ (intcode-computer-ip computer) offset) mem 0)))
    (case mode
      (0 (gethash val mem 0))
      (1 val)
      (2 (gethash (+ (intcode-computer-relative-base computer) val) mem 0))
      (t (error "I~a" mode)))))

(defun get-write-address (computer offset mode)
  (let* ((mem (intcode-computer-memory computer))
         (val (gethash (+ (intcode-computer-ip computer) offset) mem 0)))
    (case mode
      (0 val)
      (2 (+ (intcode-computer-relative-base computer) val))
      (t (error "W~a" mode)))))

(defun perform-operation (computer opcode modes)
  (destructuring-bind (mode0 mode1 mode2) modes
    (let* ((mem (intcode-computer-memory computer))
           (ip (intcode-computer-ip computer))
           (param1 (get-parameter computer 1 mode0))
           (param2 (get-parameter computer 2 mode1))
           (write-addr (get-write-address computer 3 mode2)))
      (case opcode
        (1 (setf (gethash write-addr mem 0) (+ param1 param2))
           (incf (intcode-computer-ip computer) 4))
        (2 (setf (gethash write-addr mem 0) (* param1 param2))
           (incf (intcode-computer-ip computer) 4))
        (5 (setf (intcode-computer-ip computer) (if (/= param1 0) param2 (+ ip 3))))
        (6 (setf (intcode-computer-ip computer) (if (= param1 0) param2 (+ ip 3))))
        (7 (setf (gethash write-addr mem 0) (if (< param1 param2) 1 0))
           (incf (intcode-computer-ip computer) 4))
        (8 (setf (gethash write-addr mem 0) (if (= param1 param2) 1 0))
           (incf (intcode-computer-ip computer) 4))
        (9 (incf (intcode-computer-relative-base computer) param1)
           (incf (intcode-computer-ip computer) 2))
        (t (error "O~a" opcode))))))

(defun run-intcode-step (computer input-provider)
  (loop
    (let* ((mem (intcode-computer-memory computer))
           (ip (intcode-computer-ip computer))
           (instruction (gethash ip mem 0))
           (opcode (mod instruction 100))
           (modes (loop for i from 2 to 4 collect (mod (floor instruction (expt 10 i)) 10))))

      (when (= opcode 99)
        (setf (intcode-computer-halted computer) t)
        (return (values nil :halted)))

      (destructuring-bind (mode0 mode1 mode2) modes
        (case opcode
          (3
           (let ((input-value (funcall input-provider)))
             (setf (gethash (get-write-address computer 1 mode0) mem 0) input-value)
             (incf (intcode-computer-ip computer) 2)))
          (4
           (let ((output-value (get-parameter computer 1 mode0)))
             (incf (intcode-computer-ip computer) 2)
             (return (values output-value :output))))
          (otherwise
           (perform-operation computer opcode modes)))))))

(defun play-game (program)
  (let* ((computer (make-intcode-computer))
         (ball-x 0)
         (paddle-x 0)
         (score 0))
    (loop for i from 0 below (length program)
          do (setf (gethash i (intcode-computer-memory computer) 0) (elt program i)))
    (setf (gethash 0 (intcode-computer-memory computer) 0) 2)

    (labels ((get-game-input ()
               (cond
                 ((> ball-x paddle-x) 1)
                 ((< ball-x paddle-x) -1)
                 (t 0))))

      (loop
        (multiple-value-bind (x status-x) (run-intcode-step computer #'get-game-input)
          (when (eq status-x :halted) (return score))
          (multiple-value-bind (y status-y) (run-intcode-step computer #'get-game-input)
            (when (eq status-y :halted) (return score))
            (multiple-value-bind (tile-id status-tile) (run-intcode-step computer #'get-game-input)
              (when (eq status-tile :halted) (return score))
              (cond
                ((and (= x -1) (= y 0)) (setf score tile-id))
                ((= tile-id 3) (setf paddle-x x))
                ((= tile-id 4) (setf ball-x x))))))))))

(defun simple-split (char string)
  (loop for start = 0 then (1+ end)
        for end = (position char string :start start)
        collect (subseq string start (or end (length string)))
        while end))

(defun parse-input (file-path)
  (with-open-file (stream file-path :direction :input)
    (let ((line (read-line stream nil nil)))
      (mapcar #'parse-integer (simple-split #\, line)))))

(defun main ()
  (let ((program (parse-input "input.txt")))
    (format t "~a~%" (play-game program))))

(main)
