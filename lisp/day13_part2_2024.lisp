
(defconstant +offset+ 10000000000000)

(defun string-starts-with-p (string prefix)
  (and (>= (length string) (length prefix))
       (string= string prefix :end1 (length prefix))))

(defun solve-machine (ax ay bx by px py)
  (let* ((d (- (* ax by) (* ay bx)))
         (num-a (- (* px by) (* py bx)))
         (num-b (- (* py ax) (* px ay))))
    (when (zerop d)
      (return-from solve-machine -1))
    (when (or (/= (mod num-a d) 0)
              (/= (mod num-b d) 0))
      (return-from solve-machine -1))
    (let ((a (floor num-a d))
          (b (floor num-b d)))
      (when (or (< a 0) (< b 0))
        (return-from solve-machine -1))
      (+ (* 3 a) b))))

(defun parse-coordinate-value (s)
  (parse-integer s :start (position-if #'digit-char-p s)))

(defun parse-coords (s)
  (let* ((comma-pos (position #\, s))
         (s1 (string-trim '(#\Space #\Tab #\Newline #\Return) (subseq s 0 comma-pos)))
         (s2 (string-trim '(#\Space #\Tab #\Newline #\Return) (subseq s (1+ comma-pos)))))
    (list (parse-coordinate-value s1)
          (parse-coordinate-value s2))))

(defun parse-machine-block (lines)
  (let (ax ay bx by px py)
    (loop for line in lines do
      (let ((trimmed-line (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
        (cond
          ((string-starts-with-p trimmed-line "Button A:")
           (destructuring-bind (x y) (parse-coords (subseq trimmed-line (length "Button A:")))
             (setf ax x ay y)))
          ((string-starts-with-p trimmed-line "Button B:")
           (destructuring-bind (x y) (parse-coords (subseq trimmed-line (length "Button B:")))
             (setf bx x by y)))
          ((string-starts-with-p trimmed-line "Prize:")
           (destructuring-bind (x y) (parse-coords (subseq trimmed-line (length "Prize:")))
             (setf px x py y))))))
    (list ax ay bx by px py)))

(defun read-machines (filename)
  (with-open-file (stream filename :direction :input)
    (loop with machines = nil
          with current-block-lines = nil
          for line = (read-line stream nil nil)
          while line do
            (let ((trimmed-line (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
              (if (string= trimmed-line "")
                  (progn
                    (when current-block-lines
                      (push (parse-machine-block (nreverse current-block-lines)) machines))
                    (setf current-block-lines nil))
                  (push trimmed-line current-block-lines)))
          finally
            (when current-block-lines
              (push (parse-machine-block (nreverse current-block-lines)) machines))
            (return (nreverse machines)))))

(defun main ()
  (let* ((machines (read-machines "input.txt"))
         (results nil))
    (dolist (machine machines)
      (destructuring-bind (ax ay bx by px py) machine
        (let ((cost (solve-machine ax ay bx by (+ px +offset+) (+ py +offset+))))
          (when (>= cost 0)
            (push cost results)))))
    (if results
        (format t "~d ~d~%" (length results) (apply #'+ results))
        (format t "0 0~%"))))

(main)
