
(defun parse-component-line (line)
  (let* ((pos (position #\/ line))
         (p1 (parse-integer line :end pos))
         (p2 (parse-integer line :start (1+ pos))))
    (list p1 p2)))

(defun read-components-from-file (filename)
  (with-open-file (file filename :direction :input)
    (loop for line = (read-line file nil nil)
          while line
          collect (parse-component-line line))))

(defvar *max-overall-strength* 0)
(defvar *max-overall-length* 0)

(defun build-bridge (components current-port strength length)
  (when (> length *max-overall-length*)
    (setf *max-overall-length* length)
    (setf *max-overall-strength* strength))
  (when (and (= length *max-overall-length*)
             (> strength *max-overall-strength*))
    (setf *max-overall-strength* strength))

  (loop for component in components
        do (when (member current-port component)
             (let* ((next-port (if (= (car component) current-port)
                                  (cadr component)
                                  (car component)))
                    (new-components (remove component components :test 'equal :count 1)))
               (build-bridge new-components
                             next-port
                             (+ strength (apply '+ component))
                             (+ length 1))))))

(defun main ()
  (let ((components (read-components-from-file "input.txt")))
    (setf *max-overall-strength* 0)
    (setf *max-overall-length* 0)
    (build-bridge components 0 0 0)
    (format t "~a~%" *max-overall-strength*)))

(main)
