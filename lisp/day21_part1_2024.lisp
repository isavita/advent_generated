
(defun find-position (mat ch)
  (loop for i from 0 below (length mat)
        for row = (aref mat i)
        do (loop for j from 0 below (length row)
                  when (char= (char row j) ch)
                  do (return-from find-position (values i j)))
        finally (return (values -1 -1))))

(defun ok (mat st seq)
  (destructuring-bind (curr-i curr-j) st
    (let* ((num-rows (length mat))
           (num-cols (length (aref mat 0))))
      (loop for ch across seq
            do (when (char= (char (aref mat curr-i) curr-j) #\Space)
                 (return-from ok nil))
               (case ch
                 (#\^ (decf curr-i))
                 (#\v (incf curr-i))
                 (#\< (decf curr-j))
                 (#\> (incf curr-j)))
               (when (or (< curr-i 0) (>= curr-i num-rows)
                         (< curr-j 0) (>= curr-j num-cols))
                 (return-from ok nil)))
      t)))

(defun string-replicate (char count)
  (make-string count :initial-element char))

(defun generate-moves (position objective pad)
  (multiple-value-bind (obj-i obj-j) (find-position pad objective)
    (destructuring-bind (pos-i pos-j) position
      (let* ((ret1 (concatenate 'string
                                (string-replicate #\< (max 0 (- pos-j obj-j)))
                                (string-replicate #\^ (max 0 (- pos-i obj-i)))
                                (string-replicate #\v (max 0 (- obj-i pos-i)))
                                (string-replicate #\> (max 0 (- obj-j pos-j)))))
             (ret2 (concatenate 'string
                                (string-replicate #\> (max 0 (- obj-j pos-j)))
                                (string-replicate #\^ (max 0 (- pos-i obj-i)))
                                (string-replicate #\v (max 0 (- obj-i pos-i)))
                                (string-replicate #\< (max 0 (- pos-j obj-j))))))
        (if (ok pad (list pos-i pos-j) ret1)
            ret1
            ret2)))))

(defvar *solve-memo* (make-hash-table :test 'equal))

(defun solve (code robots key-pad robot-pad max-robots)
  (when (<= robots 0)
    (return-from solve (length code)))

  (let ((state (cons code robots)))
    (multiple-value-bind (cached-val foundp) (gethash state *solve-memo*)
      (when foundp
        (return-from solve cached-val)))

    (let* ((ret 0)
           (curr-pos-i (if (= robots max-robots) 3 0))
           (curr-pos-j 2))
      (loop for ch across code
            do (let* ((target-pad (if (= robots max-robots) key-pad robot-pad))
                      (moves (generate-moves (list curr-pos-i curr-pos-j) ch target-pad)))
                 (multiple-value-setq (curr-pos-i curr-pos-j) (find-position target-pad ch))
                 (incf ret (solve (concatenate 'string moves "A")
                                  (1- robots)
                                  key-pad robot-pad max-robots))))
      (setf (gethash state *solve-memo*) ret)
      ret)))

(defun numeric-part (code)
  (let ((num 0))
    (loop for char across code
          when (digit-char-p char)
          do (setf num (+ (* num 10) (digit-char-p char 10))))
    num))

(defconstant +max-robots+ 3)
(defvar *key-pad* (vector "789" "456" "123" " 0A"))
(defvar *robot-pad* (vector " ^A" "<v>"))

(defun main ()
  (let ((total-ret 0))
    (with-open-file (f "input.txt" :direction :input)
      (loop for line = (read-line f nil)
            while line
            do (let* ((code (string-trim '(#\Space #\Newline #\Tab) line)))
                 (unless (string= code "")
                   (clrhash *solve-memo*)
                   (let ((sv (solve code +max-robots+ *key-pad* *robot-pad* +max-robots+)))
                     (incf total-ret (* sv (numeric-part code))))))))
    (format t "~a~%" total-ret)))

(main)
