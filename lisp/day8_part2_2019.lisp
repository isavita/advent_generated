
(defun decode-image (image-data width height)
  (let* ((layer-size (* width height))
         (num-layers (floor (length image-data) layer-size))
         (layers (loop for i from 0 below num-layers
                       collect (subseq image-data (* i layer-size) (+ (* i layer-size) layer-size)))))

    (let ((final-image (make-array layer-size :element-type 'character :initial-element #\ )))
      (loop for i from 0 below layer-size do
        (loop for layer in layers do
          (let ((pixel (char layer i)))
            (unless (char= pixel #\2)
              (setf (aref final-image i) (if (char= pixel #\1) #\# #\ ))
              (return)))))

      (loop for i from 0 below height do
        (loop for j from (* i width) below (+ (* i width) width) do
          (princ (aref final-image j)))
        (fresh-line)))))

(defun read-file-string (filepath)
  (with-open-file (in filepath :direction :input :if-does-not-exist :error)
    (let ((content (make-string (file-length in))))
      (read-sequence content in)
      (string-trim '(#\Space #\Tab #\Newline #\Return #\Linefeed #\Page #\VT) content))))

(defun main ()
  (let ((image-data (read-file-string "input.txt")))
    (decode-image image-data 25 6)))

(main)
