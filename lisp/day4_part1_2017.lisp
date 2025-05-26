
(defun whitespace-char-p (c)
  (member c '(#\Space #\Tab #\Newline #\Return)))

(defun split-by-whitespace (s)
  (loop for i = 0 then (1+ j)
        for start = (position-if-not #'whitespace-char-p s :start i)
        while start
        for j = (position-if #'whitespace-char-p s :start start)
        collect (subseq s start (or j (length s)))
        when (not j) do (loop-finish)))

(defun all-words-unique-p (words)
  (let ((unique-word-set (make-hash-table :test 'equal)))
    (loop for word in words
          do (setf (gethash word unique-word-set) t))
    (= (length words) (hash-table-count unique-word-set))))

(defun main ()
  (let ((valid-count 0))
    (with-open-file (stream "input.txt" :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            do (let ((words (split-by-whitespace line)))
                 (when (all-words-unique-p words)
                   (incf valid-count)))))
    (format t "~a~%" valid-count)))

(main)
