
(defun read-input-from-file (filename)
  (with-open-file (stream filename :direction :input :if-does-not-exist :error)
    (string-trim '(#\Space #\Tab #\Newline #\Return) (read-line stream nil ""))))

(defun dragon-curve (s)
  (let* ((len (length s))
         (b-chars (make-string len)))
    (loop for i from (1- len) downto 0
          for char = (char s i)
          for k from 0
          do (setf (char b-chars k) (if (char= char #\1) #\0 #\1)))
    (concatenate 'string s "0" b-chars)))

(defun generate-data (initial-state disk-length)
  (loop with data = initial-state
        while (< (length data) disk-length)
        do (setf data (dragon-curve data))
        finally (return (subseq data 0 disk-length))))

(defun calculate-checksum (data)
  (loop
    (if (oddp (length data))
        (return-from calculate-checksum data))
    (let* ((len (length data))
           (new-checksum-chars (make-string (/ len 2))))
      (loop for i from 0 by 2 below len
            for char1 = (char data i)
            for char2 = (char data (1+ i))
            for k from 0
            do (setf (char new-checksum-chars k) (if (char= char1 char2) #\1 #\0)))
      (setf data new-checksum-chars))))

(defun main ()
  (let* ((initial-state (read-input-from-file "input.txt"))
         (disk-length-1 272)
         (disk-length-2 35651584))
    (format t "~a~%" (calculate-checksum (generate-data initial-state disk-length-1)))
    (format t "~a~%" (calculate-checksum (generate-data initial-state disk-length-2)))))

(main)
