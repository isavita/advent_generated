
(defstruct row
  (springs "" :type string)
  (group nil :type list))

(defun split-string (string delimiter)
  (loop for start = 0 then (1+ pos)
        for pos = (position delimiter string :start start)
        collect (subseq string start (or pos (length string)))
        while pos))

(defun parse-string-to-ints (numbers-line)
  (mapcar #'parse-integer (split-string numbers-line #\,)))

(defun parse-input (input-lines)
  (loop for line in input-lines
        for parts = (split-string line #\ )
        for springs = (car parts)
        for group = (parse-string-to-ints (cadr parts))
        collect (make-row :springs springs :group group)))

(defun count-arrangements-recursive (row i-springs i-group i-contiguous-damaged cache)
  (let ((springs (row-springs row))
        (group (row-group row)))
    (if (= i-springs (length springs))
        (cond ((and (= i-group (length group))
                   (= i-contiguous-damaged 0)) 1)
              ((and (= i-group (1- (length group)))
                   (= i-contiguous-damaged (nth i-group group))) 1)
              (t 0))
        (let ((cache-key (list i-springs i-group i-contiguous-damaged)))
          (multiple-value-bind (res cached-p) (gethash cache-key cache)
            (if cached-p
                res
                (let ((current-res 0)
                      (char (char springs i-springs)))
                  (when (or (char= char #\.) (char= char #\?))
                    (if (= i-contiguous-damaged 0)
                        (incf current-res (count-arrangements-recursive row (1+ i-springs) i-group i-contiguous-damaged cache))
                        (when (and (< i-group (length group))
                                   (= i-contiguous-damaged (nth i-group group)))
                          (incf current-res (count-arrangements-recursive row (1+ i-springs) (1+ i-group) 0 cache)))))
                  (when (or (char= char #\#) (char= char #\?))
                    (when (and (< i-group (length group))
                               (< i-contiguous-damaged (nth i-group group)))
                      (incf current-res (count-arrangements-recursive row (1+ i-springs) i-group (1+ i-contiguous-damaged) cache))))
                  (setf (gethash cache-key cache) current-res)
                  current-res)))))))

(defun count-arrangements (row)
  (count-arrangements-recursive row 0 0 0 (make-hash-table :test 'equal)))

(defun solve (input-lines)
  (loop for row in (parse-input input-lines)
        sum (count-arrangements row)))

(defun read-file (file-name)
  (with-open-file (stream file-name :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(defun main ()
  (let* ((input-lines (read-file "input.txt"))
         (result (solve input-lines)))
    (format t "~a~%" result)))

(main)
