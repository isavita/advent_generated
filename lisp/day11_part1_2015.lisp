
(defun has-straight (s)
  (loop for i from 0 below (- (length s) 2)
        for c1 = (char-code (char s i))
        for c2 = (char-code (char s (1+ i)))
        for c3 = (char-code (char s (+ i 2)))
        when (and (= c2 (1+ c1)) (= c3 (1+ c2)))
          do (return t)
        finally (return nil)))

(defun has-no-forbidden-letters (s)
  (not (or (find #\i s) (find #\o s) (find #\l s))))

(defun has-two-non-overlapping-pairs (s)
  (let ((pair-count 0)
        (len (length s)))
    (loop for i from 0 below (1- len)
          do (when (char= (char s i) (char s (1+ i)))
               (incf pair-count)
               (incf i))
          when (>= pair-count 2)
            do (return t)
          finally (return nil))))

(defun increment-password (s)
  (let* ((len (length s))
         (chars (make-array len :element-type 'character :initial-contents s))
         (i (1- len)))
    (loop
      (cond
        ((< i 0) (return (coerce (cons #\a (coerce chars 'list)) 'string)))
        ((char= (aref chars i) #\z)
         (setf (aref chars i) #\a)
         (decf i))
        (t
         (setf (aref chars i) (code-char (1+ (char-code (aref chars i)))))
         (return (coerce chars 'string)))))))

(defun find-next-password (current-password)
  (loop
    (setf current-password (increment-password current-password))
    (when (and (has-straight current-password)
               (has-no-forbidden-letters current-password)
               (has-two-non-overlapping-pairs current-password))
      (return current-password))))

(defun main ()
  (with-open-file (file "input.txt" :direction :input :if-does-not-exist :error)
    (let* ((current-password (string-trim '(#\Newline #\Return #\Space) (read-line file))))
      (print (find-next-password current-password)))))

(main)
