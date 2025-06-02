
(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (read-line stream)))

(defparameter *direction-offsets*
  (let ((m (make-hash-table :test 'eql)))
    (setf (gethash #\N m) '(0 -1))
    (setf (gethash #\E m) '(1 0))
    (setf (gethash #\S m) '(0 1))
    (setf (gethash #\W m) '(-1 0))
    m))

(defun copy-hash-table (original-table)
  (let ((new-table (make-hash-table :test (hash-table-test original-table)
                                    :size (hash-table-size original-table))))
    (maphash (lambda (k v) (setf (gethash k new-table) v)) original-table)
    new-table))

(defun build-map (directions)
  (let* ((stack '())
         (rooms (make-hash-table :test 'equal))
         (current-room '(0 0))
         (doors-passed (make-hash-table :test 'equal)))

    (setf (gethash current-room doors-passed) 0)

    (loop for char across (subseq directions 1 (- (length directions) 1))
          do (cond
               ((char= char #\()
                (push (list current-room (copy-hash-table doors-passed)) stack))
               ((char= char #\|)
                (setf current-room (car (first stack)))
                (setf doors-passed (cadr (first stack))))
               ((char= char #\))
                (let ((popped (pop stack)))
                  (setf current-room (car popped))
                  (setf doors-passed (cadr popped))))
               (t
                (let* ((dxdy (gethash char *direction-offsets*))
                       (dx (car dxdy))
                       (dy (cadr dxdy))
                       (new-room (list (+ (car current-room) dx)
                                       (+ (cadr current-room) dy)))
                       (current-path-doors (+ (gethash current-room doors-passed) 1)))

                  (setf (gethash new-room doors-passed) current-path-doors)
                  
                  (let ((existing-doors (gethash new-room rooms)))
                    (when (or (not existing-doors)
                              (< current-path-doors existing-doors))
                      (setf (gethash new-room rooms) current-path-doors)))
                  (setf current-room new-room)))))
    rooms))

(defun main ()
  (let* ((directions (read-file "input.txt"))
         (rooms (build-map directions))
         (max-doors 0)
         (rooms-with-1000-doors 0))

    (maphash (lambda (room doors)
               (declare (ignore room))
               (when (> doors max-doors)
                 (setf max-doors doors))
               (when (>= doors 1000)
                 (incf rooms-with-1000-doors)))
             rooms)

    (princ max-doors)
    (terpri)
    (princ rooms-with-1000-doors)
    (terpri)))

(main)
