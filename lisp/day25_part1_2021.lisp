
(defun read-grid (filename)
  (with-open-file (in filename)
    (let* ((first-line (read-line in nil))
           (width (length (string-trim '(#\Newline #\Return) first-line)))
           (lines (cons first-line (loop for line = (read-line in nil)
                                         while line
                                         collect line)))
           (height (length lines))
           (grid (make-array (list height width) :element-type 'character)))
      (loop for y from 0 below height
            for line in lines
            do (loop for x from 0 below width
                     for char across (string-trim '(#\Newline #\Return) line)
                     do (setf (aref grid y x) char)))
      grid)))

(defun step-grid (current-grid)
  (let* ((height (array-dimension current-grid 0))
         (width  (array-dimension current-grid 1))
         (moved nil))

    (let ((grid-after-horizontal (make-array (list height width) :element-type 'character)))
      (loop for y below height do
        (loop for x below width do
          (setf (aref grid-after-horizontal y x) (aref current-grid y x))))

      (loop for y below height do
        (loop for x below width do
          (when (char= (aref current-grid y x) #\>)
            (let ((nx (mod (+ x 1) width)))
              (when (char= (aref current-grid y nx) #\.)
                (setf (aref grid-after-horizontal y x) #\.)
                (setf (aref grid-after-horizontal y nx) #\>)
                (setf moved t))))))

      (let ((grid-after-vertical (make-array (list height width) :element-type 'character)))
        (loop for y below height do
          (loop for x below width do
            (setf (aref grid-after-vertical y x) (aref grid-after-horizontal y x))))

        (loop for y below height do
          (loop for x below width do
            (when (char= (aref grid-after-horizontal y x) #\v)
              (let ((ny (mod (+ y 1) height)))
                (when (char= (aref grid-after-horizontal ny x) #\.)
                  (setf (aref grid-after-vertical y x) #\.)
                  (setf (aref grid-after-vertical ny x) #\v)
                  (setf moved t))))))
        
        (values grid-after-vertical moved)))))

(defun main ()
  (let ((grid (read-grid "input.txt"))
        (steps 0))
    (loop
      (multiple-value-bind (new-grid moved) (step-grid grid)
        (incf steps)
        (when (not moved)
          (format t "~A~%" steps)
          (return))
        (setf grid new-grid)))))

(main)
