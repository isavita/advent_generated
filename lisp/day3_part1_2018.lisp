
;; Define the maximum dimension for the fabric grid.
;; Based on the problem description, 1000x1000 is a safe assumption,
;; allowing coordinates from 0 to 999.
(defconstant +fabric-dimension+ 1000
  "The maximum dimension (width/height) of the fabric grid in inches.
   Assumes coordinates range from 0 to +FABRIC-DIMENSION+-1.")

;; Declare a global variable for the fabric grid.
;; It will be initialized in the main function.
(defvar *fabric-grid* nil
  "A 2D array representing the fabric. Each cell stores the number
   of claims covering that specific square inch.")

(defun parse-claim (line)
  "Parses a claim string from 'input.txt' into a list of integers:
   (ID LEFT TOP WIDTH HEIGHT).
   Example input format: #123 @ 3,2: 5x4"
  ;; Use standard string manipulation to find delimiters and parse integers.
  (let* ((at-pos (position #\@ line))
         (comma-pos (position #\, line :start at-pos))
         (colon-pos (position #\: line :start comma-pos))
         (x-pos (position #\x line :start colon-pos))
         ;; Extract and parse ID, LEFT, TOP, WIDTH, HEIGHT based on delimiter positions.
         (id (parse-integer line :start 1 :end (1- at-pos)))
         (left (parse-integer line :start (1+ at-pos) :end comma-pos))
         (top (parse-integer line :start (1+ comma-pos) :end colon-pos))
         (width (parse-integer line :start (1+ colon-pos) :end x-pos))
         (height (parse-integer line :start (1+ x-pos))))
    ;; Return the parsed values as a list.
    (list id left top width height)))

(defun main ()
  "Main entry point for the Day 3, Part 1 Advent of Code solution.
   Reads fabric claims from 'input.txt', marks them on a 2D grid,
   and then counts the number of square inches claimed by two or more elves."

  ;; 1. Initialize the fabric grid:
  ;;    Create a 2D array of the specified dimension,
  ;;    with fixed-size integer elements, initialized to 0.
  (setf *fabric-grid* (make-array (list +fabric-dimension+ +fabric-dimension+)
                                  :element-type 'fixnum
                                  :initial-element 0))

  ;; 2. Process claims from 'input.txt':
  ;;    Open the input file and read it line by line.
  (with-open-file (in "input.txt"
                      :direction :input            ; Open for reading
                      :if-does-not-exist :error) ; Signal an error if file is missing
    (loop for line = (read-line in nil nil)       ; Read a line (or NIL if EOF)
          while line                              ; Continue as long as lines are read
          do (destructuring-bind (id left top width height) (parse-claim line)
               (declare (ignore id))              ; The ID is not needed for Part 1

               ;; For each claim, iterate through all the square inches it covers
               ;; and increment the corresponding count in the fabric grid.
               (loop for x from left below (+ left width)  ; x coordinates from left to left + width - 1
                     do (loop for y from top below (+ top height) ; y coordinates from top to top + height - 1
                              do (incf (aref *fabric-grid* x y))))))) ; Increment the cell at (x, y)

  ;; 3. Count square inches claimed by two or more:
  ;;    Initialize a counter for overlaps.
  (let ((overlap-count 0))
    ;; Iterate through every cell in the fabric grid.
    (loop for x from 0 below +fabric-dimension+
          do (loop for y from 0 below +fabric-dimension+
                   do (when (>= (aref *fabric-grid* x y) 2) ; If the count is 2 or more
                        (incf overlap-count))))             ; Increment the overlap counter

    ;; 4. Print the final result to standard output.
    (format t "~a~%" overlap-count)))

;; Call the main function when this Lisp file is loaded or executed as a script.
(main)
