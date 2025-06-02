
(defpackage :intcode-vm
  (:use :cl)
  (:export :main))

(in-package :intcode-vm)

(defun split-string (string delimiter)
  (loop for start = 0 then (1+ end)
        for end = (position delimiter string :start start)
        collect (subseq string start (or end (length string)))
        while end))

(defstruct (vm (:constructor %make-vm))
  (code (make-hash-table :test 'eql) :type hash-table)
  (ip 0 :type fixnum)
  (input nil :type list)
  (output nil :type list)
  (relative-base 0 :type fixnum)
  (initial-code nil :type (or null hash-table)))

(defun copy-hash-table (ht)
  (let ((new-ht (make-hash-table :test (hash-table-test ht)
                                 :size (hash-table-size ht))))
    (maphash #'(lambda (k v) (setf (gethash k new-ht) v)) ht)
    new-ht))

(defun make-vm-from-file (filename)
  (with-open-file (f filename :direction :input)
    (let* ((line (read-line f nil nil))
           (str-codes (split-string line #\,))
           (initial-mem (make-hash-table :test 'eql)))
      (loop for s in str-codes
            for i from 0
            do (setf (gethash i initial-mem) (parse-integer s)))
      (%make-vm :initial-code initial-mem
                :code (copy-hash-table initial-mem)))))

(defun vm-reset (vm)
  (setf (vm-code vm) (copy-hash-table (vm-initial-code vm)))
  (setf (vm-ip vm) 0)
  (setf (vm-input vm) nil)
  (setf (vm-output vm) nil)
  (setf (vm-relative-base vm) 0))

(defun vm-get-mem (vm addr)
  (gethash addr (vm-code vm) 0))

(defun vm-set-mem (vm addr val)
  (setf (gethash addr (vm-code vm)) val))

(defun vm-run (vm)
  (loop
    (let* ((cmd (vm-get-mem vm (vm-ip vm)))
           (opcode (mod cmd 100))
           (modes (list (mod (floor cmd 100) 10)
                        (mod (floor cmd 1000) 10)
                        (mod (floor cmd 10000) 10))))

      (labels ((get-param (index)
                 (let* ((mode (nth (1- index) modes))
                        (val (vm-get-mem vm (+ (vm-ip vm) index))))
                   (case mode
                     (0 (vm-get-mem vm val))
                     (1 val)
                     (2 (vm-get-mem vm (+ (vm-relative-base vm) val)))
                     (t (error "Unknown parameter mode: ~a" mode)))))
               (get-address (index)
                 (let* ((mode (nth (1- index) modes))
                        (val (vm-get-mem vm (+ (vm-ip vm) index))))
                   (case mode
                     (0 val)
                     (2 (+ (vm-relative-base vm) val))
                     (t (error "Unknown address mode: ~a" mode))))))

        (case opcode
          (1
           (vm-set-mem vm (get-address 3) (+ (get-param 1) (get-param 2)))
           (incf (vm-ip vm) 4))
          (2
           (vm-set-mem vm (get-address 3) (* (get-param 1) (get-param 2)))
           (incf (vm-ip vm) 4))
          (3
           (if (null (vm-input vm))
               (error "Input buffer empty!")
               (progn
                 (vm-set-mem vm (get-address 1) (pop (vm-input vm)))
                 (incf (vm-ip vm) 2))))
          (4
           (setf (vm-output vm) (append (vm-output vm) (list (get-param 1))))
           (incf (vm-ip vm) 2))
          (5
           (if (/= (get-param 1) 0)
               (setf (vm-ip vm) (get-param 2))
               (incf (vm-ip vm) 3)))
          (6
           (if (= (get-param 1) 0)
               (setf (vm-ip vm) (get-param 2))
               (incf (vm-ip vm) 3)))
          (7
           (vm-set-mem vm (get-address 3) (if (< (get-param 1) (get-param 2)) 1 0))
           (incf (vm-ip vm) 4))
          (8
           (vm-set-mem vm (get-address 3) (if (= (get-param 1) (get-param 2)) 1 0))
           (incf (vm-ip vm) 4))
          (9
           (incf (vm-relative-base vm) (get-param 1))
           (incf (vm-ip vm) 2))
          (99
           (return))
          (t (error "Unknown opcode ~a at address ~a" opcode (vm-ip vm))))))))

(defun beam (x y vm)
  (vm-reset vm)
  (setf (vm-input vm) (list x y))
  (vm-run vm)
  (= (car (vm-output vm)) 1))

(defun main ()
  (let ((total-sum 0)
        (base-vm (make-vm-from-file "input.txt")))
    (loop for y from 0 to 49
          do (loop for x from 0 to 49
                   when (beam x y base-vm)
                   do (incf total-sum)))
    (format t "~a~%" total-sum)))

(main)
