
(defconstant +grid-size+ 1000)
(defvar *lights* (make-array (list +grid-size+ +grid-size+) :element-type 'fixnum :initial-element 0))

(defun split-string (s delim)
  (loop for i = 0 then (1+ j)
        for j = (position delim s :start i)
        collect (subseq s i (or j (length s)))
        while j))

(defun parse-coords (s)
  (mapcar #'parse-integer (split-string s #\,)))

(defun process-line (line)
  (let* ((words (split-string line #\Space))
         (cmd (car words)))
    (cond
      ((string= cmd "turn")
       (let* ((action (cadr words))
              (start-coords (parse-coords (nth 2 words)))
              (end-coords (parse-coords (nth 4 words)))
              (r1 (car start-coords))
              (c1 (cadr start-coords))
              (r2 (car end-coords))
              (c2 (cadr end-coords)))
         (loop for r from r1 to r2
               do (loop for c from c1 to c2
                        do (if (string= action "on")
                               (incf (aref *lights* r c))
                               (setf (aref *lights* r c) (max 0 (1- (aref *lights* r c)))))))))
      ((string= cmd "toggle")
       (let* ((start-coords (parse-coords (cadr words)))
              (end-coords (parse-coords (nth 3 words)))
              (r1 (car start-coords))
              (c1 (cadr start-coords))
              (r2 (car end-coords))
              (c2 (cadr end-coords)))
         (loop for r from r1 to r2
               do (loop for c from c1 to c2
                        do (incf (aref *lights* r c) 2))))))))

(defun sum-lights ()
  (loop for r from 0 below +grid-size+
        sum (loop for c from 0 below +grid-size+
                  sum (aref *lights* r c))))

(defun main ()
  (let ((filepath "input.txt"))
    (with-open-file (stream filepath :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            do (process-line line)))
    (print (sum-lights))))

(main)
