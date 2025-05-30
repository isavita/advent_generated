
(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defconstant +input-filename+ "input.txt")
(defconstant +enhancement-steps+ 2)

(defun read-input (filename)
  (with-open-file (stream filename :direction :input)
    (let* ((algorithm (read-line stream))
           (_ (read-line stream)) ; Skip blank line
           (image-lines (loop for line = (read-line stream nil nil)
                              while line
                              collect (coerce line 'list))))
      (values algorithm image-lines))))

(defun make-2d-array-from-list (list-of-lists)
  (let* ((rows (length list-of-lists))
         (cols (if (null list-of-lists) 0 (length (car list-of-lists))))
         (array (make-array (list rows cols) :element-type 'character)))
    (declare (type fixnum rows cols))
    (loop for r from 0 below rows
          for row-list in list-of-lists
          do (loop for c from 0 below cols
                   for char in row-list
                   do (setf (aref array r c) char)))
    array))

(defun calculate-index (r c image flip-char)
  (declare (type fixnum r c)
           (type (simple-array character (* *)) image)
           (type character flip-char))
  (let ((index 0)
        (rows (array-dimension image 0))
        (cols (array-dimension image 1)))
    (declare (type fixnum index rows cols))
    (loop for dr from -1 to 1
          do (loop for dc from -1 to 1
                   do (setf index (ash index 1)) ; index <<= 1
                      (let ((nr (+ r dr))
                            (nc (+ c dc)))
                        (declare (type fixnum nr nc))
                        (if (and (>= nr 0) (< nr rows)
                                 (>= nc 0) (< nc cols))
                            ;; Inside image bounds
                            (when (char= (aref image nr nc) #\#)
                              (setf index (logior index 1))) ; index |= 1
                            ;; Outside image bounds
                            (when (char= flip-char #\#)
                              (setf index (logior index 1)))))))
    index))

(defun apply-algorithm (image algorithm flip-char)
  (declare (type (simple-array character (* *)) image)
           (type simple-string algorithm)
           (type character flip-char))
  (let* ((rows (array-dimension image 0))
         (cols (array-dimension image 1))
         (new-rows (+ rows 2))
         (new-cols (+ cols 2))
         (enhanced-image (make-array (list new-rows new-cols) :element-type 'character)))
    (declare (type fixnum rows cols new-rows new-cols))
    (loop for r from 0 below new-rows
          do (loop for c from 0 below new-cols
                   do (let* ((img-r (- r 1))
                             (img-c (- c 1))
                             (index (calculate-index img-r img-c image flip-char)))
                        (declare (type fixnum img-r img-c index))
                        (setf (aref enhanced-image r c) (char algorithm index)))))
    enhanced-image))

(defun enhance-image (initial-image algorithm times)
  (declare (type list initial-image)
           (type simple-string algorithm)
           (type fixnum times))
  (let ((current-image (make-2d-array-from-list initial-image))
        (alg-first-char (char algorithm 0)))
    (declare (type (simple-array character (* *)) current-image))
    (loop for i from 0 below times
          do (let ((flip-char (if (and (= (mod i 2) 1) (char= alg-first-char #\#))
                                  #\#
                                  #\.)))
               (setf current-image (apply-algorithm current-image algorithm flip-char))))
    current-image))

(defun count-lit-pixels (image)
  (declare (type (simple-array character (* *)) image))
  (let ((count 0))
    (declare (type fixnum count))
    (loop for r from 0 below (array-dimension image 0)
          do (loop for c from 0 below (array-dimension image 1)
                   do (when (char= (aref image r c) #\#)
                        (incf count))))
    count))

(defun main ()
  (multiple-value-bind (algorithm initial-image)
      (read-input +input-filename+)
    (let* ((enhanced-image (enhance-image initial-image algorithm +enhancement-steps+))
           (lit-pixels (count-lit-pixels enhanced-image)))
      (format t "~a~%" lit-pixels))))

(main)
