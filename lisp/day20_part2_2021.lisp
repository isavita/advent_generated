
(defconstant +iterations+ 50)
(defconstant +expand-by+ 1)

(defun read-input (filename)
  (with-open-file (s filename :direction :input)
    (let* ((algorithm (read-line s))
           (_ (read-line s)) ; Skip empty line
           (image-lines (loop for line = (read-line s nil)
                              while line
                              collect line))
           (rows (length image-lines))
           (cols (if (> rows 0) (length (car image-lines)) 0))
           (image (make-array (list rows cols) :element-type 'boolean :initial-element nil)))
      (loop for r from 0 below rows
            for line in image-lines
            do (loop for c from 0 below cols
                     do (setf (aref image r c) (char= (char line c) #\#))))
      (values algorithm image))))

(defun enhance-image (algorithm image use-infinite-lit)
  (let* ((old-rows (array-dimension image 0))
         (old-cols (array-dimension image 1))
         (new-rows (+ old-rows (* +expand-by+ 2)))
         (new-cols (+ old-cols (* +expand-by+ 2)))
         (new-image (make-array (list new-rows new-cols) :element-type 'boolean :initial-element nil)))
    (loop for y from (- +expand-by+) below (+ old-rows +expand-by+)
          do (loop for x from (- +expand-by+) below (+ old-cols +expand-by+)
                   do (let ((index 0))
                        (loop for dy from -1 to 1
                              do (loop for dx from -1 to 1
                                       do (setf index (ash index 1))
                                          (let ((ny (+ y dy))
                                                (nx (+ x dx)))
                                            (cond ((and (>= ny 0) (< ny old-rows)
                                                        (>= nx 0) (< nx old-cols))
                                                   (when (aref image ny nx)
                                                     (setf index (logior index 1))))
                                                  (use-infinite-lit
                                                   (setf index (logior index 1)))))))
                        (setf (aref new-image (+ y +expand-by+) (+ x +expand-by+))
                              (char= (char algorithm index) #\#)))))
    new-image))

(defun count-lit-pixels (image)
  (let ((count 0)
        (rows (array-dimension image 0))
        (cols (array-dimension image 1)))
    (loop for r from 0 below rows
          do (loop for c from 0 below cols
                   when (aref image r c)
                     do (incf count)))
    count))

(defun main ()
  (multiple-value-bind (algorithm image) (read-input "input.txt")
    (loop for i from 0 below +iterations+
          do (setf image (enhance-image algorithm
                                        image
                                        (and (oddp i) (char= (char algorithm 0) #\#)))))
    (format t "~a~%" (count-lit-pixels image))))

(main)
