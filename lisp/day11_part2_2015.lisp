
(defconstant +invalid-chars+ '(#\i #\o #\l))

(defun read-input (filename)
  (with-open-file (f filename :direction :input)
    (read-line f)))

(defun increment-password (password)
  (let* ((len (length password))
         (chars (coerce password 'list))
         (carry t))
    (loop for i from (1- len) downto 0
          when carry do
            (let ((next-code (1+ (char-code (nth i chars)))))
              (if (> next-code (char-code #\z))
                  (setf (nth i chars) #\a)
                  (progn (setf (nth i chars) (code-char next-code))
                         (setf carry nil)))))
    (coerce chars 'string)))

(defun has-straight (password)
  (loop for i from 0 below (- (length password) 2)
        for c1 = (char password i)
        for c2 = (char password (1+ i))
        for c3 = (char password (+ i 2))
        when (and (= (1+ (char-code c1)) (char-code c2))
                  (= (1+ (char-code c2)) (char-code c3)))
          return t))

(defun contains-invalid-letters (password)
  (loop for char across password
        when (member char +invalid-chars+)
          return t))

(defun has-two-pairs (password)
  (let ((count 0)
        (i 0)
        (len (length password)))
    (loop while (< i (1- len))
          do (if (char= (char password i) (char password (1+ i)))
                 (progn (incf count) (incf i 2))
                 (incf i)))
    (>= count 2)))

(defun is-valid-password (password)
  (and (has-straight password)
       (not (contains-invalid-letters password))
       (has-two-pairs password)))

(defun find-next-password (password)
  (loop
    (setf password (increment-password password))
    (when (is-valid-password password)
      (return password))))

(defun main ()
  (let* ((current-password (read-input "input.txt"))
         (first-new-password (find-next-password current-password))
         (second-new-password (find-next-password first-new-password)))
    (format t "~a~%" second-new-password)))

(main)
