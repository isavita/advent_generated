
(defun get-round-score (opponent player)
  (let ((shape-score (case player
                       (#\X 1)
                       (#\Y 2)
                       (#\Z 3)))
        (outcome-keyword (case opponent
                           (#\A (case player (#\X :draw) (#\Y :win) (#\Z :lose)))
                           (#\B (case player (#\X :lose) (#\Y :draw) (#\Z :win)))
                           (#\C (case player (#\X :win) (#\Y :lose) (#\Z :draw))))))
    (+ shape-score
       (case outcome-keyword
         (:win 6)
         (:draw 3)
         (:lose 0)))))

(defun calculate-total-score (filename)
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          sum (let* ((opponent (char line 0))
                     (player (char line 2)))
                (get-round-score opponent player)))))

(defun main ()
  (format t "~a~%" (calculate-total-score "input.txt")))

(main)
