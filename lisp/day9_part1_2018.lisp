
(defstruct marble
  (value 0 :type integer)
  (next nil :type (or null marble))
  (prev nil :type (or null marble)))

(defun make-circular-marble (value)
  (let ((m (make-marble :value value)))
    (setf (marble-next m) m)
    (setf (marble-prev m) m)
    m))

(defun play-marble-game (players last-marble)
  (let ((scores (make-array players :initial-element 0 :element-type 'integer))
        (current-marble (make-circular-marble 0)))
    (loop for i from 1 to last-marble do
      (if (= (mod i 23) 0)
          (progn
            (incf (aref scores (mod i players)) i)
            (loop repeat 7 do
              (setf current-marble (marble-prev current-marble)))
            (incf (aref scores (mod i players)) (marble-value current-marble))
            (let ((prev-node (marble-prev current-marble))
                  (next-node (marble-next current-marble)))
              (setf (marble-next prev-node) next-node)
              (setf (marble-prev next-node) prev-node)
              (setf current-marble next-node)))
          (progn
            (setf current-marble (marble-next current-marble))
            (let* ((new-marble (make-marble :value i))
                   (next-node (marble-next current-marble)))
              (setf (marble-next new-marble) next-node)
              (setf (marble-prev new-marble) current-marble)
              (setf (marble-next current-marble) new-marble)
              (setf (marble-prev next-node) new-marble)
              (setf current-marble new-marble)))))
    (loop for score across scores maximize score)))

(defun read-input (filename)
  (with-open-file (f filename :direction :input)
    (let* ((line (read-line f nil nil))
           (p-space-idx (position #\Space line))
           (players (parse-integer (subseq line 0 p-space-idx)))
           (marble-word-idx (search "marble is worth " line))
           (points-word-idx (search " points" line :start2 (+ marble-word-idx (length "marble is worth "))))
           (last-marble (parse-integer (subseq line (+ marble-word-idx (length "marble is worth ")) points-word-idx))))
      (values players last-marble))))

(defun main ()
  (multiple-value-bind (players last-marble) (read-input "input.txt")
    (format t "~a~%" (play-marble-game players last-marble))))

(main)
