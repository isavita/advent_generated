
;; For the environment, this code is written for Common Lisp.
;; Standard functions like SQRT, CEILING, ABS, EVENP, etc. are used.

(defun part1 (n)
  "Calculates the Manhattan Distance from square N to square 1 in the spiral memory."
  (if (= n 1)
      0 ; Base case: square 1 is at (0,0), distance 0
      (let* ((s (ceiling (sqrt n))) ; Find smallest integer side length of square that encloses N
             ;; Adjust s to be the smallest odd side length:
             ;; If s is even (e.g., for N=12, sqrt(12)=3.46 -> ceil=4), it means N
             ;; is part of the spiral that *ends* on an odd-sided square.
             ;; For example, square 9 is 3x3, square 25 is 5x5.
             (s (if (evenp s) (1+ s) s))
             (r (/ (1- s) 2)) ; 'radius' or max absolute coordinate for this s-sized square
             (sq-val (* s s)) ; The largest value in this s-sized square, located at (r, -r)
             (offset (- sq-val n)) ; Steps back from sq-val to reach n
             (x 0) (y 0))
        
        ;; Determine (x, y) coordinates of N based on its position in the outermost ring.
        ;; The spiral moves: Left from (r, -r), then Up from (-r, -r), then Right from (-r, r), then Down from (r, r).
        ;; Each segment between corners has length (s-1).
        (cond
          ((< offset (1- s)) ; N is on the bottom segment (moving left)
           (setf x (- r offset))
           (setf y (- r)))
          ((< offset (* 2 (1- s))) ; N is on the left segment (moving up)
           (setf offset (- offset (1- s)))
           (setf x (- r))
           (setf y (+ (- r) offset)))
          ((< offset (* 3 (1- s))) ; N is on the top segment (moving right)
           (setf offset (- offset (* 2 (1- s))))
           (setf x (+ (- r) offset))
           (setf y r))
          (t ; N is on the right segment (moving down)
           (setf offset (- offset (* 3 (1- s))))
           (setf x r)
           (setf y (- r offset))))
        
        ;; Manhattan Distance from (x,y) to (0,0) is |x| + |y|
        (+ (abs x) (abs y)))))

(defun part2 (target-sum)
  "Finds the first value written to the spiral grid that is larger than TARGET-SUM,
   where each square's value is the sum of its adjacent (including diagonal) filled squares."
  (let ((grid (make-hash-table :test 'equal)) ; Hash table to store values at (x . y) coordinates
        (curr-x 0) (curr-y 0)
        (dx 1) (dy 0)           ; Initial direction: (1,0) (right)
        (move-length 1)         ; Number of steps to take in the current direction before turning
        (moves-made 0)          ; Steps taken in the current direction for current move-length
        (turns-at-current-length 0)) ; Number of turns made since move-length was last incremented

    ;; Square 1 starts with the value 1
    (setf (gethash (cons 0 0) grid) 1)

    ;; Loop to generate the spiral values
    (loop
      ;; Move to the next square according to current direction
      (incf curr-x dx)
      (incf curr-y dy)

      (let ((sum 0))
        ;; Calculate the sum of values in all 8 adjacent squares (including diagonals)
        (loop for ix from -1 to 1
              do (loop for iy from -1 to 1
                       when (or (/= ix 0) (/= iy 0)) ; Exclude the current square (0,0) offset itself
                         do (incf sum (gethash (cons (+ curr-x ix) (+ curr-y iy)) grid 0)))) ; Default to 0 if key not found
        
        ;; Store the calculated sum in the grid
        (setf (gethash (cons curr-x curr-y) grid) sum)

        ;; Check if this sum exceeds the target-sum
        (when (> sum target-sum)
          (return sum))) ; If it does, this is our answer, return it and exit the loop

      ;; Update movement parameters for the spiral
      (incf moves-made)
      (when (= moves-made move-length) ; If we've moved the current move-length steps
        (setf moves-made 0) ; Reset steps taken for the new direction
        (incf turns-at-current-length) ; Count this turn
        
        ;; Change direction: Right -> Up -> Left -> Down -> Right ...
        (cond
          ((and (= dx 1) (= dy 0)) (setf dx 0 dy 1))   ; Was moving Right, now move Up
          ((and (= dx 0) (= 1 dy)) (setf dx -1 dy 0))  ; Was moving Up, now move Left
          ((and (= dx -1) (= dy 0)) (setf dx 0 dy -1)) ; Was moving Left, now move Down
          ((and (= dx 0) (= -1 dy)) (setf dx 1 dy 0))) ; Was moving Down, now move Right
        
        ;; After every two turns (e.g., Right and Up), increase the move-length
        (when (= turns-at-current-length 2)
          (incf move-length)
          (setf turns-at-current-length 0))))))

(defun main ()
  "Main entry point of the program. Reads input from 'input.txt',
   solves Part 1 and Part 2, and prints the results."
  (let ((input-file "input.txt"))
    ;; Use handler-case for robust file opening
    (handler-case
        (with-open-file (in input-file :direction :input :if-does-not-exist :error)
          (let ((n (read in))) ; Reads the integer from the file
            (format t "Input N: ~a~%" n)
            (format t "Part 1 result: ~a~%" (part1 n))
            (format t "Part 2 result: ~a~%" (part2 n))))
      ;; Catch file-specific errors
      (file-error (c)
        (format *error-output* "Error: Could not open file ~a - ~a~%" input-file c))
      ;; Catch general errors (e.g., if input.txt contains non-numeric data)
      (error (c)
        (format *error-output* "An unexpected error occurred: ~a~%" c)))))

;; The following form ensures that `main` is called when the file is loaded
;; and executed in a Common Lisp environment (e.g., SBCL, CCL).
(main)
