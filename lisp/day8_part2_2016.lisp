
(defun split-string (s delimiter)
  (loop for i = 0 then (1+ j)
        for j = (position delimiter s :start i)
        collect (subseq s i (or j (length s)))
        while j))

(defconstant +screen-rows+ 6)
(defconstant +screen-cols+ 50)

(defun make-screen ()
  (loop repeat +screen-rows+
        collect (loop repeat +screen-cols+ collect #\.)))

(defun rect (screen width height)
  (loop for i from 0 below height
        do (loop for j from 0 below width
                 do (setf (elt (elt screen i) j) #\#))))

(defun rotate-row (screen row-idx by)
  (let* ((row (elt screen row-idx))
         (len +screen-cols+)
         (shift-idx (mod (- len by) len)))
    (setf (elt screen row-idx)
          (append (nthcdr shift-idx row) (subseq row 0 shift-idx)))))

(defun rotate-column (screen col-idx by)
  (let ((temp-col (loop for i from 0 below +screen-rows+
                        collect (elt (elt screen i) col-idx))))
    (loop for i from 0 below +screen-rows+
          do (setf (elt (elt screen (mod (+ i by) +screen-rows+)) col-idx)
                   (elt temp-col i)))))

(defun apply-instruction (screen instruction)
  (let* ((parts (split-string instruction #\Space))
         (command (car parts)))
    (cond
      ((string= command "rect")
       (let* ((dims (split-string (cadr parts) #\x))
              (width (parse-integer (car dims)))
              (height (parse-integer (cadr dims))))
         (rect screen width height)))
      ((string= command "rotate")
       (let* ((type (cadr parts))
              (idx-part (caddr parts))
              (idx (parse-integer (subseq idx-part (1+ (position #\= idx-part)))))
              (by (parse-integer (nth 4 parts))))
         (cond
           ((string= type "row")
            (rotate-row screen idx by))
           ((string= type "column")
            (rotate-column screen idx by))))))))

(defun print-screen (screen)
  (loop for row in screen
        do (format t "~{~A~}~%" row)))

(defun main ()
  (let ((screen (make-screen)))
    (with-open-file (file "input.txt" :direction :input)
      (loop for line = (read-line file nil)
            while line
            do (apply-instruction screen line)))
    (print-screen screen)))

(main)
