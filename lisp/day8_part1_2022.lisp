
(defun parse-line (line)
  (loop for char across line
        collect (digit-char-p char)))

(defun get-grid-dimensions (grid)
  (values (length grid) (length (car grid))))

(defun grid-ref (grid r c)
  (nth c (nth r grid)))

(defun is-visible (r c grid rows cols)
  (let* ((height (grid-ref grid r c))
         (directions '((-1 0) (1 0) (0 -1) (0 1))))
    (loop for dir in directions
          do (destructuring-bind (dr dc) dir
               (let ((nr (+ r dr))
                     (nc (+ c dc))
                     (blocked nil))
                 (loop
                   (unless (and (>= nr 0) (< nr rows) (>= nc 0) (< nc cols))
                     (return))
                   (when (>= (grid-ref grid nr nc) height)
                     (setf blocked t)
                     (return))
                   (incf nr dr)
                   (incf nc dc))
                 (unless blocked
                   (return-from is-visible t)))))
    nil))

(defun count-visible-trees (grid)
  (multiple-value-bind (rows cols) (get-grid-dimensions grid)
    (let ((visible-count 0))
      (loop for r from 0 to (- rows 1)
            do (loop for c from 0 to (- cols 1)
                     do (when (or (= r 0) (= r (- rows 1))
                                  (= c 0) (= c (- cols 1))
                                  (is-visible r c grid rows cols))
                          (incf visible-count))))
      visible-count)))

(defun main ()
  (let* ((input-path "input.txt")
         (grid (with-open-file (s input-path :direction :input)
                 (loop for line = (read-line s nil)
                       while line
                       collect (parse-line line)))))
    (format t "~a~%" (count-visible-trees grid))))

(main)
