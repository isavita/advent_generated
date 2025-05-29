
(defun read-file-lines (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun split-string (delimiter-string s)
  (let ((pos (search delimiter-string s)))
    (if pos
        (list (subseq s 0 pos) (subseq s (+ pos (length delimiter-string))))
        (list s ""))))

(defun parse-rules (lines)
  (let ((rules (make-hash-table :test 'equal)))
    (loop for line in (cddr lines)
          do (let* ((parts (split-string " -> " line))
                    (pair (first parts))
                    (insertion (second parts)))
               (setf (gethash pair rules) insertion)))
    rules))

(defun polymerize (template rules steps)
  (loop with current = template
        for step from 1 to steps
        do (setf current
                 (with-output-to-string (s)
                   (loop for i from 0 below (1- (length current))
                         for pair = (subseq current i (+ i 2))
                         for insertion = (gethash pair rules)
                         do (write-char (char current i) s)
                            (when insertion
                              (write-string insertion s)))
                   (write-char (char current (1- (length current))) s)))
        finally (return current)))

(defun count-elements (polymer)
  (let ((counts (make-hash-table :test 'equal)))
    (loop for char across polymer
          do (setf (gethash (string char) counts)
                   (1+ (gethash (string char) counts 0))))
    counts))

(defun main ()
  (let* ((input-lines (read-file-lines "input.txt"))
         (template (first input-lines))
         (rules (parse-rules input-lines))
         (polymer (polymerize template rules 10))
         (counts (count-elements polymer)))
    (let ((min-val most-positive-fixnum)
          (max-val 0))
      (maphash (lambda (key value)
                 (declare (ignore key))
                 (setf min-val (min min-val value))
                 (setf max-val (max max-val value)))
               counts)
      (format t "~a~%" (- max-val min-val)))))

(main)
