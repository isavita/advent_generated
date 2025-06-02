
(in-package :cl-user)

(defun split-string (string delimiter)
  (loop for start = 0 then (+ end 1)
        for end = (position delimiter string :start start)
        collect (subseq string start (if end end (length string)))
        while end))

(defun decode (n)
  (let ((op (mod n 100)))
    (list op
          (list (mod (floor n 100) 10)
                (mod (floor n 1000) 10)
                (mod (floor n 10000) 10)))))

(defstruct machine
  (data (make-hash-table :test 'eql))
  (ip 0)
  (in-stream nil)
  (out-stream nil)
  (relbase 0))

(defun machine-mem-get (machine addr)
  (gethash addr (machine-data machine) 0))

(defun (setf machine-mem-get) (value machine addr)
  (setf (gethash addr (machine-data machine)) value))

(defun machine-get (machine i mo)
  (let ((val (machine-mem-get machine i)))
    (case mo
      (0 (machine-mem-get machine val))
      (1 val)
      (2 (machine-mem-get machine (+ (machine-relbase machine) val))))))

(defun machine-set (machine i mo val)
  (let ((addr-val (machine-mem-get machine i)))
    (case mo
      (0 (setf (machine-mem-get machine addr-val) val))
      (2 (setf (machine-mem-get machine (+ (machine-relbase machine) addr-val)) val)))))

(defun machine-step (m)
  (let* ((ip (machine-ip m))
         (instr (machine-mem-get m ip))
         (decoded (decode instr))
         (op (car decoded))
         (modes (cadr decoded))
         (mode0 (nth 0 modes))
         (mode1 (nth 1 modes))
         (mode2 (nth 2 modes)))
    (case op
      (1
       (let* ((val1 (machine-get m (+ ip 1) mode0))
              (val2 (machine-get m (+ ip 2) mode1)))
         (machine-set m (+ ip 3) mode2 (+ val1 val2))
         (incf (machine-ip m) 4)))
      (2
       (let* ((val1 (machine-get m (+ ip 1) mode0))
              (val2 (machine-get m (+ ip 2) mode1)))
         (machine-set m (+ ip 3) mode2 (* val1 val2))
         (incf (machine-ip m) 4)))
      (3
       (let* ((in-val (car (machine-in-stream m))))
         (machine-set m (+ ip 1) mode0 in-val)
         (setf (machine-in-stream m) (cdr (machine-in-stream m)))
         (incf (machine-ip m) 2)))
      (4
       (let ((out-val (machine-get m (+ ip 1) mode0)))
         (push out-val (machine-out-stream m))
         (incf (machine-ip m) 2)))
      (5
       (let* ((test-val (machine-get m (+ ip 1) mode0)))
         (if (/= test-val 0)
             (setf (machine-ip m) (machine-get m (+ ip 2) mode1))
             (incf (machine-ip m) 3))))
      (6
       (let* ((test-val (machine-get m (+ ip 1) mode0)))
         (if (= test-val 0)
             (setf (machine-ip m) (machine-get m (+ ip 2) mode1))
             (incf (machine-ip m) 3))))
      (7
       (let* ((val1 (machine-get m (+ ip 1) mode0))
              (val2 (machine-get m (+ ip 2) mode1)))
         (machine-set m (+ ip 3) mode2 (if (< val1 val2) 1 0))
         (incf (machine-ip m) 4)))
      (8
       (let* ((val1 (machine-get m (+ ip 1) mode0))
              (val2 (machine-get m (+ ip 2) mode1)))
         (machine-set m (+ ip 3) mode2 (if (= val1 val2) 1 0))
         (incf (machine-ip m) 4)))
      (9
       (let ((offset (machine-get m (+ ip 1) mode0)))
         (incf (machine-relbase m) offset)
         (incf (machine-ip m) 2)))
      (99 nil)
      (t (error "Unknown opcode: ~a at ip ~a" op ip)))))

(defun machine-run (m)
  (loop while (machine-step m)))

(defun run-intcode (program input-list)
  (let ((data-ht (make-hash-table :test 'eql)))
    (loop for val in program
          for i from 0
          do (setf (gethash i data-ht) val))
    (let ((m (make-machine :data data-ht :in-stream input-list)))
      (machine-run m)
      (nreverse (machine-out-stream m)))))

(defun parse-scaffolding (program)
  (let* ((output (run-intcode program nil))
         (scaffolding (make-hash-table :test 'equal))
         (robot nil)
         (dir-map '((#\^ . 0) (#\> . 1) (#\v . 2) (#\< . 3)))
         (robot-dir nil)
         (x 0)
         (y 0))
    (loop for o in output
          do (let ((c (code-char o)))
               (cond ((char= c #\Newline)
                      (incf y)
                      (setf x 0))
                     ((find c "^v<>" :test 'char=)
                      (setf robot (list x y))
                      (setf robot-dir (cdr (assoc c dir-map :test 'char=)))
                      (setf (gethash (list x y) scaffolding) #\#)
                      (incf x))
                     ((char= c #\#)
                      (setf (gethash (list x y) scaffolding) #\#)
                      (incf x))
                     (t (incf x)))))
    (list scaffolding robot robot-dir)))

(defun sum-align (grid)
  (let ((sum 0)
        (neighbors '((0 1) (0 -1) (1 0) (-1 0))))
    (maphash (lambda (coord value)
               (declare (ignore value))
               (let ((x (car coord))
                     (y (cadr coord)))
                 (when (every (lambda (dxy)
                                (destructuring-bind (dx dy) dxy
                                  (gethash (list (+ x dx) (+ y dy)) grid)))
                              neighbors)
                   (incf sum (* x y)))))
             grid)
    sum))

(defun main ()
  (with-open-file (f "input.txt" :direction :input)
    (let* ((line (read-line f))
           (program (mapcar #'parse-integer (split-string line #\,))))
      (destructuring-bind (scaffolding robot dir) (parse-scaffolding program)
        (declare (ignore robot dir))
        (princ (sum-align scaffolding))
        (terpri)))))

(main)
