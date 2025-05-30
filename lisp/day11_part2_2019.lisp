
(defclass intcode-computer ()
  ((memory :initform (make-hash-table) :accessor computer-memory)
   (ip :initform 0 :accessor computer-ip)
   (relative-base :initform 0 :accessor computer-relative-base)
   (halted :initform nil :accessor computer-halted)))

(defmethod initialize-instance :after ((computer intcode-computer) &key program)
  (loop for val in program
        for i from 0
        do (setf (gethash i (computer-memory computer)) val)))

(defun get-param (computer mode offset)
  (let* ((param-addr (+ (computer-ip computer) offset))
         (param (gethash param-addr (computer-memory computer) 0)))
    (case mode
      (0 (gethash param (computer-memory computer) 0))
      (1 param)
      (2 (gethash (+ (computer-relative-base computer) param) (computer-memory computer) 0))
      (t (error "Unknown parameter mode: ~a" mode)))))

(defun set-param (computer mode offset value)
  (let* ((param-addr (+ (computer-ip computer) offset))
         (param (gethash param-addr (computer-memory computer) 0))
         (address nil))
    (case mode
      (0 (setf address param))
      (2 (setf address (+ (computer-relative-base computer) param)))
      (t (error "Unknown parameter mode for writing: ~a" mode)))
    (setf (gethash address (computer-memory computer)) value)))

(defmethod run-computer ((computer intcode-computer) &optional input-value)
  (loop
    (when (computer-halted computer)
      (return (values :halted nil)))

    (let* ((instruction (gethash (computer-ip computer) (computer-memory computer) 0))
           (opcode (mod instruction 100))
           (mode1 (mod (floor instruction 100) 10))
           (mode2 (mod (floor instruction 1000) 10))
           (mode3 (mod (floor instruction 10000) 10)))

      (case opcode
        (1 (let ((p1 (get-param computer mode1 1))
                 (p2 (get-param computer mode2 2)))
             (set-param computer mode3 3 (+ p1 p2))
             (incf (computer-ip computer) 4)))
        (2 (let ((p1 (get-param computer mode1 1))
                 (p2 (get-param computer mode2 2)))
             (set-param computer mode3 3 (* p1 p2))
             (incf (computer-ip computer) 4)))
        (3 (if (not (null input-value))
               (progn
                 (set-param computer mode1 1 input-value)
                 (incf (computer-ip computer) 2)
                 (setf input-value nil))
               (return (values :need-input nil))))
        (4 (let ((output-val (get-param computer mode1 1)))
             (incf (computer-ip computer) 2)
             (return (values :output output-val))))
        (5 (let ((p1 (get-param computer mode1 1))
                 (p2 (get-param computer mode2 2)))
             (setf (computer-ip computer) (if (/= p1 0) p2 (+ (computer-ip computer) 3)))))
        (6 (let ((p1 (get-param computer mode1 1))
                 (p2 (get-param computer mode2 2)))
             (setf (computer-ip computer) (if (= p1 0) p2 (+ (computer-ip computer) 3)))))
        (7 (let ((p1 (get-param computer mode1 1))
                 (p2 (get-param computer mode2 2)))
             (set-param computer mode3 3 (if (< p1 p2) 1 0))
             (incf (computer-ip computer) 4)))
        (8 (let ((p1 (get-param computer mode1 1))
                 (p2 (get-param computer mode2 2)))
             (set-param computer mode3 3 (if (= p1 p2) 1 0))
             (incf (computer-ip computer) 4)))
        (9 (let ((p1 (get-param computer mode1 1)))
             (incf (computer-relative-base computer) p1)
             (incf (computer-ip computer) 2)))
        (99 (progn
              (setf (computer-halted computer) t)
              (return (values :halted nil))))
        (t (error "Unknown opcode: ~a" opcode))))))

(defclass robot ()
  ((computer :accessor robot-computer)
   (direction :initform 0 :accessor robot-direction)
   (position :initform (list 0 0) :accessor robot-position)
   (panels :initform (make-hash-table :test 'equal) :accessor robot-panels)
   (painted-panels :initform (make-hash-table :test 'equal) :accessor robot-painted-panels)))

(defmethod initialize-instance :after ((robot robot) &key program start-panel-color)
  (setf (robot-computer robot) (make-instance 'intcode-computer :program program))
  (setf (gethash (get-position-key robot) (robot-panels robot)) start-panel-color))

(defun get-position-key (robot)
  (format nil "~a,~a" (first (robot-position robot)) (second (robot-position robot))))

(defun turn-and-move (robot turn-direction)
  (let* ((current-dir (robot-direction robot))
         (new-dir (if (= turn-direction 0)
                      (mod (+ current-dir 3) 4)
                      (mod (+ current-dir 1) 4)))
         (x (first (robot-position robot)))
         (y (second (robot-position robot))))
    (setf (robot-direction robot) new-dir)
    (setf (robot-position robot)
          (case new-dir
            (0 (list x (1- y)))
            (1 (list (1+ x) y))
            (2 (list x (1+ y)))
            (3 (list (1- x) y))
            (t (error "Invalid direction: ~a" new-dir))))))

(defmethod run-robot ((robot robot))
  (loop
    (multiple-value-bind (signal output-value)
        (run-computer (robot-computer robot) (gethash (get-position-key robot) (robot-panels robot) 0))
      (case signal
        (:halted (return))
        (:need-input nil)
        (:output (let ((paint-color output-value))
                   (multiple-value-bind (next-signal turn-direction)
                       (run-computer (robot-computer robot))
                     (unless (eq next-signal :output)
                       (error "Expected turn direction output, got ~a" next-signal))
                     (setf (gethash (get-position-key robot) (robot-panels robot)) paint-color)
                     (setf (gethash (get-position-key robot) (robot-painted-panels robot)) t)
                     (turn-and-move robot turn-direction))))
        (t (error "Unknown signal from computer: ~a" signal))))))

(defmethod get-painted-panels-count ((robot robot))
  (hash-table-count (robot-painted-panels robot)))

(defun split-string (string delimiter)
  (loop for start = 0 then (1+ end)
        for end = (position delimiter string :start start)
        collect (subseq string start (or end (length string)))
        while end))

(defmethod render-panels ((robot robot))
  (let ((min-x 0) (max-x 0) (min-y 0) (max-y 0))
    (unless (zerop (hash-table-count (robot-painted-panels robot)))
      (maphash (lambda (key val)
                 (declare (ignore val))
                 (let* ((parts (mapcar #'parse-integer (split-string key #\,)))
                        (x (first parts))
                        (y (second parts)))
                   (setf min-x (min min-x x))
                   (setf max-x (max max-x x))
                   (setf min-y (min min-y y))
                   (setf max-y (max max-y y))))
               (robot-painted-panels robot)))

    (format t "~%Registration Identifier:~%")
    (loop for y from min-y to max-y
          do (loop for x from min-x to max-x
                   do (princ (if (= (gethash (format nil "~a,~a" x y) (robot-panels robot) 0) 1)
                                 "#"
                                 " ")))
             (terpri))))

(defun parse-input (filepath)
  (with-open-file (f filepath :direction :input :if-does-not-exist :error)
    (let ((line (read-line f nil nil)))
      (unless line (error "Input file is empty"))
      (mapcar #'parse-integer (split-string line #\,)))))

(defun main ()
  (let* ((input-file "input.txt")
         (program (parse-input input-file)))

    (let ((robot-part1 (make-instance 'robot :program program :start-panel-color 0)))
      (run-robot robot-part1)
      (format t "Part One: ~a panels painted at least once.~%" (get-painted-panels-count robot-part1)))

    (let ((robot-part2 (make-instance 'robot :program (copy-list program) :start-panel-color 1)))
      (run-robot robot-part2)
      (format t "Part Two: Registration identifier painted on the hull.~%")
      (render-panels robot-part2))))

(main)
