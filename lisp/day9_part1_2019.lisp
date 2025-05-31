
(defun split-string-by-char (string char)
  (loop with results = '()
        with current-start = 0
        for i from 0 to (length string)
        when (or (= i (length string)) (char= (char string i) char))
        do (push (subseq string current-start i) results)
           (setf current-start (1+ i))
        finally (return (nreverse results))))

(defun read-file-content (filename)
  (with-open-file (stream filename :direction :input :if-does-not-exist :error)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      content)))

(defun parse-input (input-string)
  (let ((memory (make-hash-table)))
    (loop for val-str in (split-string-by-char (string-trim '(#\Newline #\Return #\Space) input-string) #\,)
          for idx from 0
          do (setf (gethash idx memory) (parse-integer val-str)))
    memory))

(defun get-mem (memory address)
  (gethash address memory 0))

(defun set-mem (memory address value)
  (setf (gethash address memory) value))

(defun run-intcode (memory)
  (let ((ip 0)
        (relative-base 0)
        (output 0))

    (labels ((get-mode (modes-int offset)
               (case offset
                 (1 (mod modes-int 10))
                 (2 (mod (floor modes-int 10) 10))
                 (3 (mod (floor modes-int 100) 10))
                 (otherwise (error "Invalid offset for mode"))))

             (get-param (offset modes-int)
               (let* ((mode (get-mode modes-int offset))
                      (param-val (get-mem memory (+ ip offset))))
                 (case mode
                   (0 (get-mem memory param-val))
                   (1 param-val)
                   (2 (get-mem memory (+ relative-base param-val)))
                   (otherwise (error "Unknown parameter mode for get-param")))))

             (set-param (offset value modes-int)
               (let* ((mode (get-mode modes-int offset))
                      (param-addr (get-mem memory (+ ip offset))))
                 (case mode
                   (0 (set-mem memory param-addr value))
                   (2 (set-mem memory (+ relative-base param-addr) value))
                   (otherwise (error "Unknown parameter mode for set-param for write"))))))

      (loop
        (let* ((instruction (get-mem memory ip))
               (opcode (mod instruction 100))
               (modes-int (floor instruction 100)))

          (case opcode
            (1
             (set-param 3 (+ (get-param 1 modes-int) (get-param 2 modes-int)) modes-int)
             (incf ip 4))
            (2
             (set-param 3 (* (get-param 1 modes-int) (get-param 2 modes-int)) modes-int)
             (incf ip 4))
            (3
             (set-param 1 1 modes-int)
             (incf ip 2))
            (4
             (setf output (get-param 1 modes-int))
             (incf ip 2))
            (5
             (if (/= (get-param 1 modes-int) 0)
                 (setf ip (get-param 2 modes-int))
                 (incf ip 3)))
            (6
             (if (= (get-param 1 modes-int) 0)
                 (setf ip (get-param 2 modes-int))
                 (incf ip 3)))
            (7
             (set-param 3 (if (< (get-param 1 modes-int) (get-param 2 modes-int)) 1 0) modes-int)
             (incf ip 4))
            (8
             (set-param 3 (if (= (get-param 1 modes-int) (get-param 2 modes-int)) 1 0) modes-int)
             (incf ip 4))
            (9
             (incf relative-base (get-param 1 modes-int))
             (incf ip 2))
            (99
             (return output))
            (otherwise
             (error "Unknown opcode: ~a at address ~a" opcode ip))))))))

(defun main ()
  (let* ((input-content (read-file-content "input.txt"))
         (initial-memory (parse-input input-content)))
    (format t "~a~%" (run-intcode initial-memory))))

(main)
