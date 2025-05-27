
;; Define the main entry point for the program
(defun main ()
  "Reads coordinates from input.txt, solves the Chronal Coordinates problem,
   and prints the largest finite area to standard output."
  (format t "~a~%" (solve)))

;; Solves the Chronal Coordinates problem by simulating grid points.
(defun solve ()
  "Calculates the size of the largest finite area around the given coordinates."
  ;; Read coordinates from the specified file. Each is (ID X Y).
  (let* ((coords (read-input "input.txt"))
         ;; Determine the bounding box of the input coordinates.
         (min-x (apply #'min (mapcar #'second coords)))
         (max-x (apply #'max (mapcar #'second coords)))
         (min-y (apply #'min (mapcar #'third coords)))
         (max-y (apply #'max (mapcar #'third coords)))

         ;; Define an expanded grid for processing. Points on this outer
         ;; boundary will mark their closest coordinate's area as infinite.
         (grid-min-x (- min-x 1))
         (grid-max-x (+ max-x 1))
         (grid-min-y (- min-y 1))
         (grid-max-y (+ max-y 1))

         ;; Hash table to keep track of IDs of coordinates with infinite areas.
         ;; Key: coordinate ID, Value: T (if infinite)
         (infinite-ids (make-hash-table))

         ;; Hash table to store the area size for each coordinate ID.
         ;; Key: coordinate ID, Value: current count of closest points.
         (area-sizes (make-hash-table)))

    ;; Initialize area sizes to 0 for all input coordinates.
    (loop for (id x y) in coords do
      (setf (gethash id area-sizes) 0))

    ;; Iterate through every integer point (x, y) within the expanded grid.
    (loop for x from grid-min-x to grid-max-x do
      (loop for y from grid-min-y to grid-max-y do
        (let ((min-dist most-positive-fixnum) ; Stores the minimum Manhattan distance found.
              (closest-id nil)                ; Stores the ID of the coordinate closest to (x,y).
              (tie-p nil))                    ; True if multiple coordinates are equally closest.

          ;; Calculate Manhattan distance from (x,y) to all input coordinates.
          (loop for (id coord-x coord-y) in coords do
            (let ((dist (manhattan-distance x y coord-x coord-y)))
              (cond
                ((< dist min-dist)
                 ;; Found a new minimum distance.
                 (setf min-dist dist
                       closest-id id
                       tie-p nil))
                ((= dist min-dist)
                 ;; Found a tie for the minimum distance.
                 (setf tie-p t)))))

          ;; If a unique closest coordinate is found for (x,y):
          (when (and closest-id (not tie-p))
            ;; Increment the area size for this coordinate.
            (incf (gethash closest-id area-sizes))

            ;; If this point (x,y) is on the outer boundary of the expanded grid,
            ;; it means the area for 'closest-id' extends infinitely.
            (when (or (= x grid-min-x) (= x grid-max-x)
                      (= y grid-min-y) (= y grid-max-y))
              (setf (gethash closest-id infinite-ids) t))))))

    ;; After processing all points, find the largest area among the finite ones.
    (let ((max-finite-area 0))
      (maphash (lambda (id size)
                 ;; Only consider areas that have not been marked as infinite.
                 (unless (gethash id infinite-ids)
                   (setf max-finite-area (max max-finite-area size))))
               area-sizes)
      max-finite-area)))

;; Calculates the Manhattan distance between two points (x1, y1) and (x2, y2).
(defun manhattan-distance (x1 y1 x2 y2)
  "Computes the Manhattan distance between (x1, y1) and (x2, y2)."
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

;; Reads coordinates from the specified file.
;; Each line is assumed to be in the format "X, Y".
;; Returns a list of (ID X Y) lists.
(defun read-input (filename)
  "Reads coordinates from the specified file. Each line is 'X, Y'.
   Returns a list of (ID X Y) lists, where ID is the 0-indexed line number."
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil) ; Read line by line until EOF.
          for id from 0                         ; Assign a unique ID to each coordinate.
          while line
          collect (let* ((comma-pos (position #\, line))
                         ;; Extract X string (trimming leading/trailing spaces).
                         (x-str (string-trim " " (subseq line 0 comma-pos)))
                         ;; Extract Y string (trimming leading/trailing spaces).
                         (y-str (string-trim " " (subseq line (1+ comma-pos)))))
                    ;; Parse to integers and store as (ID X Y).
                    (list id (parse-integer x-str) (parse-integer y-str))))))

;; Execute the main function when the script is loaded or run.
;; This is a standard Common Lisp idiom to define the program's entry point.
(eval-when (:load-toplevel :execute)
  (main))
