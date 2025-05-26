
(defun parse-lock (block)
  (loop for c from 0 below 5
        collect (loop for r from 1 below 7
                      for char = (char (elt block r) c)
                      count (char= char #\#)
                      while (char= char #\#))))

(defun parse-key (block)
  (loop for c from 0 below 5
        collect (loop for r from 5 downto 0
                      for char = (char (elt block r) c)
                      count (char= char #\#)
                      while (char= char #\#))))

(defun fits (lock key)
  (loop for i from 0 below 5
        always (<= (+ (elt lock i) (elt key i)) 5)))

(defun main ()
  (let* ((raw-lines (with-open-file (s "input.txt")
                      (loop for line = (read-line s nil nil)
                            while line
                            for trimmed-line = (string-trim '(#\Space #\Tab #\Newline #\Return) line)
                            when (not (string= trimmed-line ""))
                              collect trimmed-line))))
    (unless (zerop (mod (length raw-lines) 7))
      (print 0)
      (return-from main))

    (let ((locks nil)
          (keys nil))
      (loop for i from 0 below (length raw-lines) by 7
            do (let ((block (subseq raw-lines i (+ i 7))))
                 (unless (some #'(lambda (ln) (< (length ln) 5)) block)
                   (if (loop for char across (elt block 0) always (char= char #\#))
                       (push (parse-lock block) locks)
                       (push (parse-key block) keys)))))
      (setf locks (nreverse locks))
      (setf keys (nreverse keys))

      (print (loop for lock in locks
                   sum (loop for key in keys
                             count (fits lock key)))))))

(main)
