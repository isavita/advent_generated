
(defconstant +directions+
  '((1 0 0) (-1 0 0)
    (0 1 0) (0 -1 0)
    (0 0 1) (0 0 -1)))

(defun parse-coordinates (line)
  (let* ((first-comma (position #\, line))
         (second-comma (position #\, line :start (1+ first-comma))))
    (list (parse-integer line :start 0 :end first-comma)
          (parse-integer line :start (1+ first-comma) :end second-comma)
          (parse-integer line :start (1+ second-comma)))))

(defun read-input (filepath)
  (let ((cubes (make-hash-table :test 'equal)))
    (with-open-file (stream filepath :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            do (setf (gethash (parse-coordinates line) cubes) t)))
    cubes))

(defun calculate-surface-area (cubes)
  (let ((surface-area 0))
    (maphash (lambda (cube-coords value)
               (declare (ignore value))
               (let* ((x (first cube-coords))
                      (y (second cube-coords))
                      (z (third cube-coords))
                      (exposed-sides 6))
                 (loop for dir in +directions+ do
                   (let* ((dx (first dir))
                          (dy (second dir))
                          (dz (third dir))
                          (neighbor-coords (list (+ x dx) (+ y dy) (+ z dz))))
                     (when (gethash neighbor-coords cubes)
                       (decf exposed-sides))))
                 (incf surface-area exposed-sides)))
             cubes)
    surface-area))

(defun main ()
  (let* ((cubes (read-input "input.txt"))
         (surface-area (calculate-surface-area cubes)))
    (format t "~a~%" surface-area)))

(main)
