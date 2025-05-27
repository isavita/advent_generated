
;;; Day 8: Two-Factor Authentication - Screen Simulation

;; Global constants for screen dimensions
(defconstant SCREEN-WIDTH 50
  "The width (number of columns) of the screen.")
(defconstant SCREEN-HEIGHT 6
  "The height (number of rows) of the screen.")

;; The screen is represented as a 2D array of booleans (T for lit, NIL for off).
;; Declared as a global variable (using `defvar`) to be accessible by operation functions.
(defvar *screen* nil
  "The 2D array representing the screen pixels. T for lit, NIL for off.")

;; Initializes the screen to all off pixels.
(defun initialize-screen ()
  "Initializes the global *screen* array to its default state (all pixels off)."
  (setf *screen* (make-array (list SCREEN-HEIGHT SCREEN-WIDTH) :initial-element nil)))

;; Implements the "rect AxB" operation.
;; Turns on all pixels in a rectangle at the top-left of the screen.
;; `width` is A, `height` is B.
(defun rect-op (width height)
  "Turns on pixels in a rectangle from (0,0) up to (width-1, height-1)."
  (loop for y from 0 below height do
    (loop for x from 0 below width do
      (setf (aref *screen* y x) t))))

;; Implements the "rotate row y=A by B" operation.
;; Shifts all pixels in row `row-idx` right by `shift-by` pixels.
;; Pixels wrapping off the right appear at the left.
(defun rotate-row-op (row-idx shift-by)
  "Cyclically shifts row `row-idx` to the right by `shift-by` positions."
  (let* ((actual-shift (mod shift-by SCREEN-WIDTH)) ; Normalize shift amount
         (temp-row (make-array SCREEN-WIDTH)))
    ;; Copy the current row's pixels to a temporary array
    (loop for x from 0 below SCREEN-WIDTH do
      (setf (aref temp-row x) (aref *screen* row-idx x)))
    ;; Write the shifted pixels back to the screen
    (loop for x from 0 below SCREEN-WIDTH do
      ;; The pixel from `temp-row` at index `x` moves to the destination index
      ;; `(x + actual-shift) mod SCREEN-WIDTH` in the *screen* array.
      (setf (aref *screen* row-idx (mod (+ x actual-shift) SCREEN-WIDTH))
            (aref temp-row x)))))

;; Implements the "rotate column x=A by B" operation.
;; Shifts all pixels in column `col-idx` down by `shift-by` pixels.
;; Pixels wrapping off the bottom appear at the top.
(defun rotate-col-op (col-idx shift-by)
  "Cyclically shifts column `col-idx` down by `shift-by` positions."
  (let* ((actual-shift (mod shift-by SCREEN-HEIGHT)) ; Normalize shift amount
         (temp-col (make-array SCREEN-HEIGHT)))
    ;; Copy the current column's pixels to a temporary array
    (loop for y from 0 below SCREEN-HEIGHT do
      (setf (aref temp-col y) (aref *screen* y col-idx)))
    ;; Write the shifted pixels back to the screen
    (loop for y from 0 below SCREEN-HEIGHT do
      ;; The pixel from `temp-col` at index `y` moves to the destination index
      ;; `(y + actual-shift) mod SCREEN-HEIGHT` in the *screen* array.
      (setf (aref *screen* (mod (+ y actual-shift) SCREEN-HEIGHT) col-idx)
            (aref temp-col y)))))

;; Parses an instruction line and executes the corresponding operation.
(defun parse-instruction (line)
  "Parses a single instruction line and calls the appropriate screen operation function."
  (cond
    ((string= (subseq line 0 4) "rect")
     ;; Format: "rect AxB"
     (let* ((x-pos (position #\x line))
            (width (parse-integer (subseq line 5 x-pos)))
            (height (parse-integer (subseq line (+ x-pos 1)))))
       (rect-op width height)))
    ((search "rotate row" line :start2 0)
     ;; Format: "rotate row y=A by B"
     (let* ((y-start (search "y=" line))
            (by-start (search " by " line :start2 y-start))
            (row-idx (parse-integer (subseq line (+ y-start 2) by-start)))
            (shift-start (+ by-start 4)) ; Skip " by " to get to the shift value
            (shift-by (parse-integer (subseq line shift-start))))
       (rotate-row-op row-idx shift-by)))
    ((search "rotate column" line :start2 0)
     ;; Format: "rotate column x=A by B"
     (let* ((x-start (search "x=" line))
            (by-start (search " by " line :start2 x-start))
            (col-idx (parse-integer (subseq line (+ x-start 2) by-start)))
            (shift-start (+ by-start 4)) ; Skip " by " to get to the shift value
            (shift-by (parse-integer (subseq line shift-start))))
       (rotate-col-op col-idx shift-by)))
    (t
     (error "Unknown instruction format: ~a" line))))

;; Counts the number of lit pixels on the screen.
(defun count-lit-pixels ()
  "Counts and returns the total number of lit (T) pixels on the screen."
  (let ((count 0))
    (loop for y from 0 below SCREEN-HEIGHT do
      (loop for x from 0 below SCREEN-WIDTH do
        (when (aref *screen* y x)
          (incf count))))
    count))

;; Main entry point of the program.
;; Reads instructions from input.txt, processes them, and prints the count of lit pixels.
(defun main ()
  "The main function of the program. Initializes the screen, reads and processes
   instructions from 'input.txt', and prints the final count of lit pixels."
  (initialize-screen) ; Set up a fresh screen before processing instructions

  ;; Read instructions from input.txt line by line
  (with-open-file (stream "input.txt" :direction :input)
    (loop for line = (read-line stream nil) ; Read a line; `nil` means return NIL on EOF
          while line do ; Continue as long as a line is successfully read
      (parse-instruction line))) ; Process the current instruction line

  ;; Print the final count of lit pixels to standard output
  (format t "~a~%" (count-lit-pixels)))

;; Call the main function when the script is loaded/executed.
;; #+sbcl allows for a clean exit status when run as an executable on SBCL.
#+sbcl (sb-ext:exit :code (progn (main) 0))
#-sbcl (main) ; Standard way to run the main function on other CL implementations

;; Optional: A function to print the screen for debugging purposes.
(defun print-screen ()
  "Prints the current state of the screen to standard output,
   using '#' for lit pixels and '.' for off pixels."
  (loop for y from 0 below SCREEN-HEIGHT do
    (loop for x from 0 below SCREEN-WIDTH do
      (princ (if (aref *screen* y x) #\# #\.)))
    (terpri)) ; Newline after each row
  (terpri)) ; Extra newline for visual separation
