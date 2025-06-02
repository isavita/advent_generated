
(defconstant +origin+ '(0 0))

(defun move (p dir)
  (destructuring-bind (x y) p
    (case dir
      (#\N (list x (1- y)))
      (#\S (list x (1+ y)))
      (#\E (list (1+ x) y))
      (#\W (list (1- x) y))
      (t p))))

(defun build-map (regex)
  (let ((dm (make-hash-table :test 'equal))
        (stack '())
        (cp +origin+))
    (loop for char across regex
          do (case char
               (#\( (push cp stack))
               (#\| (setf cp (car stack)))
               (#\) (setf cp (pop stack)))
               (t
                (let ((np (move cp char)))
                  (let ((cp-neighbors (gethash cp dm (make-hash-table :test 'equal))))
                    (setf (gethash np cp-neighbors) t)
                    (setf (gethash cp dm) cp-neighbors))
                  (let ((np-neighbors (gethash np dm (make-hash-table :test 'equal))))
                    (setf (gethash cp np-neighbors) t)
                    (setf (gethash np dm) np-neighbors))
                  (setf cp np)))))
    dm))

(defun find-furthest-room (dm)
  (let ((visited (make-hash-table :test 'equal))
        (queue (list +origin+))
        (max-doors 0))
    (setf (gethash +origin+ visited) 0)
    (loop while queue
          do (let* ((p (pop queue))
                    (current-doors (gethash p visited)))
               (loop for np being the hash-keys of (gethash p dm)
                     do (unless (gethash np visited)
                          (let ((new-doors (1+ current-doors)))
                            (setf (gethash np visited) new-doors)
                            (setf max-doors (max max-doors new-doors))
                            (setf queue (nconc queue (list np))))))))
    max-doors))

(defun main ()
  (with-open-file (file "input.txt" :direction :input)
    (let* ((regex (string-trim '(#\^ #\$) (read-line file)))
           (dm (build-map regex))
           (max-doors (find-furthest-room dm)))
      (format t "~a~%" max-doors))))

(main)
