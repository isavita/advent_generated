
(declaim (optimize (speed 3) (safety 1) (debug 1))) ; Common optimization flags for good performance and some debugging info

;; Global variables to hold the state of the simulation
(defvar *track-grid* nil "2D array storing track characters. Indexed as (row y, col x).")
(defvar *carts* nil "List of CART structs, representing all active carts.")
(defvar *cart-positions* nil "Hash table mapping (x . y) coordinates to cart IDs for efficient collision detection.")
(defvar *max-x* 0 "Maximum X coordinate (width of track grid).")
(defvar *max-y* 0 "Maximum Y coordinate (height of track grid).")

;; Define the CART struct to hold cart-specific data
(defstruct (cart
            ;; Custom print method for easier debugging
            (:print-object (lambda (obj stream)
                             (format stream "#<CART ID ~a (~a,~a) Dir (~a,~a) Turn ~a>"
                                     (cart-id obj) (cart-x obj) (cart-y obj)
                                     (cart-dx obj) (cart-dy obj) (cart-turn-state obj)))))
  (id 0 :type fixnum)         ; Unique identifier for the cart
  (x 0 :type fixnum)          ; Current X coordinate
  (y 0 :type fixnum)          ; Current Y coordinate
  (dx 0 :type (member -1 0 1)) ; X-component of direction: -1 (left), 0 (none), 1 (right)
  (dy 0 :type (member -1 0 1)) ; Y-component of direction: -1 (up), 0 (none), 1 (down)
  (turn-state 0 :type (mod 3))) ; State for intersection turns: 0 (left), 1 (straight), 2 (right), modulo 3

;; Helper function to rotate a direction vector
(defun rotate-direction (dx dy turn-type)
  "Rotates the (dx, dy) direction vector based on turn-type (:left, :right, :straight).
   Returns two values: new-dx and new-dy."
  (declare (type (member -1 0 1) dx dy)
           (type (member :left :right :straight) turn-type))
  (case turn-type
    (:left  (values dy (- dx))) ; Example: (0, -1) [Up] -> (-1, 0) [Left]
    (:right (values (- dy) dx)) ; Example: (0, -1) [Up] -> (1, 0) [Right]
    (:straight (values dx dy)))) ; No change, return original values

;; Helper function to retrieve a character from the track grid
(defun get-track (x y)
  "Retrieves the track character at (x, y) from the global *track-grid*.
   Y corresponds to the row index, and X to the column index."
  (declare (type fixnum x y))
  (the character (aref *track-grid* y x)))

;; Function to parse the input file and initialize the simulation state
(defun parse-input (filename)
  (setf *carts* nil) ; Clear any existing carts from previous runs
  (setf *max-x* 0)   ; Reset max dimensions
  (setf *max-y* 0)

  (let ((raw-lines nil)
        (current-y 0))
    ;; First pass: Read lines to determine grid dimensions (max width and height)
    (with-open-file (stream filename :direction :input)
      (loop for line = (read-line stream nil nil) ; Read line by line
            while line                           ; Stop when end of file is reached
            do (push line raw-lines)             ; Store line for second pass
               (setf *max-x* (max *max-x* (length line))) ; Update max-x if current line is longer
               (incf current-y)))               ; Increment line count for max-y
    (setf *max-y* current-y)
    (setf raw-lines (nreverse raw-lines)) ; Reverse lines to process from top (Y=0) to bottom

    ;; Initialize track grid with #\Space characters to handle irregular line lengths
    (setf *track-grid* (make-array (list *max-y* *max-x*) :element-type 'character :initial-element #\Space))
    ;; Initialize hash table for collision tracking (keys are (x . y) conses, requiring #'equal test)
    (setf *cart-positions* (make-hash-table :test #'equal)) 

    ;; Second pass: Populate the grid and create cart objects
    (let ((cart-id-counter 0))
      (loop for y from 0 below *max-y*          ; Iterate through rows (Y coordinates)
            for line = (nth y raw-lines)         ; Get the current line
            do (loop for x from 0 below (length line) ; Iterate through characters in the line (X coordinates)
                     for char = (char line x)    ; Get the character at current (x, y)
                     do (case char
                          (#\^ (push (make-cart :id (incf cart-id-counter) :x x :y y :dx 0 :dy -1) *carts*)
                               (setf (aref *track-grid* y x) #\|)) ; Cart facing up, track is |
                          (#\v (push (make-cart :id (incf cart-id-counter) :x x :y y :dx 0 :dy 1) *carts*)
                               (setf (aref *track-grid* y x) #\|)) ; Cart facing down, track is |
                          (#\< (push (make-cart :id (incf cart-id-counter) :x x :y y :dx -1 :dy 0) *carts*)
                               (setf (aref *track-grid* y x) #\-)) ; Cart facing left, track is -
                          (#\> (push (make-cart :id (incf cart-id-counter) :x x :y y :dx 1 :dy 0) *carts*)
                               (setf (aref *track-grid* y x) #\-)) ; Cart facing right, track is -
                          (t (setf (aref *track-grid* y x) char)))))) ; Regular track character
    (setf *carts* (nreverse *carts*)) ; Restore original order of carts (carts found earlier are earlier in the list)
    ))

;; Main simulation logic to find the first collision
(defun solve ()
  "Simulates cart movement tick by tick and prints the coordinates of the first collision.
   This function runs in an infinite loop until a collision is detected."
  (loop
    ;; 1. Sort carts for processing order (top-to-bottom, then left-to-right)
    (let ((sorted-carts (sort (copy-list *carts*) ; Create a copy to sort, preserving original *carts* order for next tick
                              #'(lambda (c1 c2)
                                  (declare (type cart c1 c2))
                                  (if (= (cart-y c1) (cart-y c2))
                                      (< (cart-x c1) (cart-x c2))
                                      (< (cart-y c1) (cart-y c2)))))))
      
      ;; 2. Rebuild *cart-positions* hash table with current positions of ALL carts
      ;; This hash table will be dynamically updated as carts move within the tick.
      (clrhash *cart-positions*)
      (dolist (cart *carts*)
        (declare (type cart cart))
        (setf (gethash (cons (cart-x cart) (cart-y cart)) *cart-positions*) (cart-id cart)))

      ;; 3. Move each cart in the sorted order
      (dolist (cart sorted-carts)
        (declare (type cart cart))
        ;; Remove cart from its current position in the hash table BEFORE moving.
        ;; This frees up its old spot and is crucial for correct collision detection.
        (remhash (cons (cart-x cart) (cart-y cart)) *cart-positions*)

        ;; Calculate the cart's new position
        (let* ((current-dx (cart-dx cart))
               (current-dy (cart-dy cart))
               (new-x (+ (cart-x cart) current-dx))
               (new-y (+ (cart-y cart) current-dy)))
          (declare (type fixnum new-x new-y current-dx current-dy))

          ;; Collision check: Check if the new position is already occupied by another cart.
          ;; This includes carts that haven't moved yet in this tick, or carts that just moved
          ;; to this spot earlier in this tick.
          (when (gethash (cons new-x new-y) *cart-positions*)
            (format t "~a,~a~%" new-x new-y) ; Print collision coordinates
            (return-from solve)) ; Collision detected, terminate simulation

          ;; Update cart's position to the new coordinates
          (setf (cart-x cart) new-x
                (cart-y cart) new-y)

          ;; Determine cart's new direction based on the track type at the new position
          (let ((track-char (get-track new-x new-y)))
            (declare (type character track-char))
            (case track-char
              (#\/
               ;; Curve /: changes direction based on incoming path
               (cond
                 ((and (= current-dx 0) (= current-dy -1)) (setf (cart-dx cart) 1 (cart-dy cart) 0)) ; Up -> Right
                 ((and (= current-dx 0) (= current-dy 1)) (setf (cart-dx cart) -1 (cart-dy cart) 0)) ; Down -> Left
                 ((and (= current-dx -1) (= current-dy 0)) (setf (cart-dx cart) 0 (cart-dy cart) 1)) ; Left -> Down
                 ((and (= current-dx 1) (= current-dy 0)) (setf (cart-dx cart) 0 (cart-dy cart) -1)) ; Right -> Up
                 (t (error "Invalid direction (~a,~a) for / at ~a,~a" current-dx current-dy new-x new-y))))
              (#\\
               ;; Curve \: changes direction based on incoming path
               (cond
                 ((and (= current-dx 0) (= current-dy -1)) (setf (cart-dx cart) -1 (cart-dy cart) 0)) ; Up -> Left
                 ((and (= current-dx 0) (= current-dy 1)) (setf (cart-dx cart) 1 (cart-dy cart) 0)) ; Down -> Right
                 ((and (= current-dx -1) (= current-dy 0)) (setf (cart-dx cart) 0 (cart-dy cart) -1)) ; Left -> Up
                 ((and (= current-dx 1) (= current-dy 0)) (setf (cart-dx cart) 0 (cart-dy cart) 1)) ; Right -> Down
                 (t (error "Invalid direction (~a,~a) for \\ at ~a,~a" current-dx current-dy new-x new-y))))
              (#\+
               ;; Intersection: Turn left, then straight, then right, then repeat
               (let ((turn-state (cart-turn-state cart)))
                 (case turn-state
                   (0 (setf (values (cart-dx cart) (cart-dy cart)) (rotate-direction current-dx current-dy :left)))
                   (1 nil) ; Straight, no change in direction
                   (2 (setf (values (cart-dx cart) (cart-dy cart)) (rotate-direction current-dx current-dy :right))))
                 (setf (cart-turn-state cart) (mod (+ turn-state 1) 3)))) ; Cycle turn state
              ((#\| #\-) nil) ; Straight tracks, no change in direction
              (t (error "Unknown track character: ~a at ~a,~a" track-char new-x new-y))))

          ;; Add cart to its new position in the hash table, claiming the spot
          (setf (gethash (cons new-x new-y) *cart-positions*) (cart-id cart)))))))

;; Main entry point function for the program
(defun main ()
  "Reads input from 'input.txt', simulates cart movement, and prints the first collision coordinates.
   This function serves as the primary entry point for the program."
  (parse-input "input.txt") ; Initialize track grid and carts from the input file
  (solve))                   ; Start the simulation

;; Execute the main function when this Lisp file is loaded or run.
;; For a production environment, especially with SBCL, you might wrap this in a call
;; to (sb-ext:save-lisp-and-die) with :toplevel and :executable options to create a standalone binary.
(main)
