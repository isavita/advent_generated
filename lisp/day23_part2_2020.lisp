
(defparameter *total-cups* 1000000)
(defparameter *total-moves* 10000000)
(defvar *cups* nil)

(defun main ()
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0)))
  (let* ((input-string (with-open-file (file "input.txt" :direction :input)
                         (read-line file nil nil)))
         (input-length (length input-string))
         (first-cup (digit-char-p (char input-string 0)))
         (last-cup 0)
         (current-cup first-cup))

    (setf *cups* (make-array (1+ *total-cups*) :initial-element 0))

    (loop for i from 0 below input-length
          for char-digit = (char input-string i)
          for cup = (digit-char-p char-digit)
          do (when (> i 0)
               (setf (aref *cups* last-cup) cup))
             (setf last-cup cup))

    (loop for i from (1+ input-length) to *total-cups*
          do (setf (aref *cups* last-cup) i)
             (setf last-cup i))

    (setf (aref *cups* last-cup) first-cup)

    (loop for move from 1 to *total-moves*
          do
             (let* ((pickup1 (aref *cups* current-cup))
                    (pickup2 (aref *cups* pickup1))
                    (pickup3 (aref *cups* pickup2)))

               (setf (aref *cups* current-cup) (aref *cups* pickup3))

               (let ((destination-cup (if (> current-cup 1)
                                          (- current-cup 1)
                                          *total-cups*)))
                 (loop
                   (when (and (/= destination-cup pickup1)
                              (/= destination-cup pickup2)
                              (/= destination-cup pickup3))
                     (return))
                   (setf destination-cup (if (> destination-cup 1)
                                             (- destination-cup 1)
                                             *total-cups*)))

                 (setf (aref *cups* pickup3) (aref *cups* destination-cup))
                 (setf (aref *cups* destination-cup) pickup1)))

             (setf current-cup (aref *cups* current-cup)))

    (let* ((cup1 (aref *cups* 1))
           (cup2 (aref *cups* cup1)))
      (print (* cup1 cup2)))))

(main)
