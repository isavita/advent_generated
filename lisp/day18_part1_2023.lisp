
;; Define a simple string splitting function (similar to `uiop:split-string` but manual)
(defun simple-split-string (string delimiter)
  "Splits a string by a single character delimiter."
  (loop for i = 0 then (1+ j)
        as j = (position delimiter string :start i)
        collect (subseq string i (if j j (length string)))
        while j))

(defun parse-line (line)
  "Parses a line like 'R 6 (#70c710)' into direction char and distance integer."
  (let* ((parts (simple-split-string line #\Space)))
    (values (char (first parts) 0)           ; Direction char (e.g., #\R)
            (parse-integer (second parts))   ; Distance integer (e.g., 6)
            ;; Color string (e.g., "70c710") - not used in Part 1
            (subseq (third parts) 1 (1- (length (third parts))))))) ; Remove leading (# and trailing )

(defun solve-day18 (filename)
  "Calculates the total cubic meters of lava the lagoon can hold."
  (let ((current-x 0)
        (current-y 0)
        ;; Store vertices as cons cells (x . y) for efficiency and common practice.
        ;; Start with (0 . 0) - the digger's initial position.
        (vertices (list (cons 0 0)))
        (perimeter-length 0))

    (with-open-file (stream filename :direction :input)
      (loop for line = (read-line stream nil nil)
            while line do
              (multiple-value-bind (dir dist color) (parse-line line)
                (declare (ignore color)) ; Color is not relevant for Part 1
                (incf perimeter-length dist)
                (case dir
                  (#\R (incf current-x dist))
                  (#\L (decf current-x dist))
                  (#\U (decf current-y dist)) ; Y decreases when moving Up (typical grid/screen coords)
                  (#\D (incf current-y dist))) ; Y increases when moving Down
                ;; Add the new vertex to the list. `push` adds to front, so reverse later.
                (push (cons current-x current-y) vertices))))
    
    ;; Vertices were pushed, so they are in reverse order. Correct order is needed for Shoelace.
    (setf vertices (nreverse vertices))

    ;; Calculate Shoelace Area: Sum (x_i * y_{i+1}) - Sum (y_i * x_{i+1}) / 2
    (let ((sum-xy-next 0) ; Sum of x_i * y_{i+1}
          (sum-yx-next 0) ; Sum of y_i * x_{i+1}
          (num-vertices (length vertices)))
      
      ;; Iterate through all segments (v_i, v_{i+1}) including the closing segment (v_n, v_0)
      (loop for i from 0 below num-vertices do
        (let* ((p1 (nth i vertices))
               (p2 (nth (mod (1+ i) num-vertices) vertices)) ; Next vertex (wraps around to first)
               (x1 (car p1))
               (y1 (cdr p1))
               (x2 (car p2))
               (y2 (cdr p2)))
          (incf sum-xy-next (* x1 y2))
          (incf sum-yx-next (* y1 x2))))
      
      ;; Shoelace formula gives the area of the polygon.
      ;; For integer coordinates and a closed loop on a grid, `abs(- sum-xy-next sum-yx-next)`
      ;; will always be an even number, so the area will be an integer.
      (let ((shoelace-area (abs (/ (- sum-xy-next sum-yx-next) 2))))
        ;; Apply Pick's Theorem: Total Area = Interior Points + Boundary Points / 2 + 1
        ;; The problem asks for total cubic meters, which is Interior Cells + Boundary Cells.
        ;; Based on Pick's: I = Area - B/2 + 1.
        ;; Total Cells = I + B = (Area - B/2 + 1) + B = Area + B/2 + 1.
        ;; 'perimeter-length' is the total length of the boundary, which corresponds to B.
        (+ shoelace-area (/ perimeter-length 2) 1)))))

(defun main ()
  "Main entry point for the program. Reads from input.txt and prints the result."
  (format t "~A~%" (solve-day18 "input.txt")))

;; Call the main function when the script is loaded/run.
(main)
