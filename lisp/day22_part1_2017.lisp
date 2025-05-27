
;; Sporifica Virus - Advent of Code 2017 Day 22 Part 1

;; --- Constants for Directions ---
;; Directions are represented by integers:
;; 0: Up, 1: Right, 2: Down, 3: Left
(defconstant +dir-up+ 0)
(defconstant +dir-right+ 1)
(defconstant +dir-down+ 2)
(defconstant +dir-left+ 3)

;; Precomputed delta values for x and y coordinates based on direction.
;; Indexed using the direction constants.
(defconstant +dx+ #(0 1 0 -1)) ; Change in x for each direction
(defconstant +dy+ #(-1 0 1 0)) ; Change in y for each direction

;; --- Helper Functions for Carrier Movement ---

(declaim (inline turn-right turn-left)) ; Optimize these small functions

(defun turn-right (current-dir)
  "Turns the carrier 90 degrees right from its current direction."
  (mod (1+ current-dir) 4))

(defun turn-left (current-dir)
  "Turns the carrier 90 degrees left from its current direction.
   Common Lisp's MOD handles negative numbers correctly (e.g., (mod -1 4) => 3)."
  (mod (1- current-dir) 4))

;; --- Input Reading Function ---

(defun read-input (filename)
  "Reads the initial grid from the specified file and returns a hash table.
   Keys are (cons x y) representing coordinates (x is column, y is row).
   Values are :infected. Clean nodes are not stored explicitly.
   The origin (0,0) in the hash table corresponds to the center of the input map."
  (let ((grid (make-hash-table :test 'equal)) ; Use 'equal for comparing (cons x y) keys
        (lines nil)
        (width 0)
        (height 0))
    ;; First pass: Read all lines to determine dimensions (width and height)
    (with-open-file (f filename :direction :input)
      (loop for line = (read-line f nil nil)
            while line
            do (push line lines)
               (unless (zerop (length line))
                 (setf width (length line))) ; Assume all lines have the same width
               (incf height)))

    (setf lines (nreverse lines)) ; Restore original order (lines were pushed in reverse)

    ;; Calculate the center offset for coordinate mapping.
    ;; The center of the input map (e.g., (1,1) for a 3x3 map) becomes (0,0) in our system.
    (let ((center-x (floor width 2))
          (center-y (floor height 2)))
      ;; Second pass: Populate the hash table with initially infected nodes
      (loop for y from 0 below height
            for line in lines
            do (loop for x from 0 below width
                     for char = (char line x)
                     when (char= char #\#)
                       ;; Map input coordinates to our relative grid system.
                       ;; E.g., if center-x=1, input x=0 becomes -1, x=1 becomes 0, x=2 becomes 1.
                       do (setf (gethash (cons (- x center-x)
                                                (- y center-y))
                                         grid)
                                :infected))))
    grid))

;; --- Main Simulation Logic ---

(defun solve-part1 (filename num-bursts)
  "Simulates the Sporifica Virus carrier's movement and infection process.
   Returns the total number of bursts that caused a node to become infected."
  (let* ((grid (read-input filename))
         (carrier-x 0)              ; Carrier starts at (0,0) relative to the map center
         (carrier-y 0)
         (carrier-dir +dir-up+)     ; Carrier starts facing up
         (infection-count 0))       ; Counter for new infections

    ;; Main simulation loop
    (loop for burst from 1 to num-bursts
          do (let* ((current-pos (cons carrier-x carrier-y)) ; Current node's coordinates
                    ;; Get the status of the current node.
                    ;; If the node is not in the hash table, it's considered :clean.
                    (node-status (gethash current-pos grid :clean)))

               ;; 1. Turn: Adjust carrier's direction based on current node's status
               (setf carrier-dir
                     (case node-status
                       (:infected (turn-right carrier-dir))
                       (:clean    (turn-left carrier-dir))))

               ;; 2. Infect/Clean: Change the node's status
               (case node-status
                 (:clean
                   ;; Node was clean, now becomes infected. Increment infection count.
                   (setf (gethash current-pos grid) :infected)
                   (incf infection-count))
                 (:infected
                   ;; Node was infected, now becomes clean.
                   (setf (gethash current-pos grid) :clean))))

             ;; 3. Move: Advance the carrier one node in the new direction
             (incf carrier-x (aref +dx+ carrier-dir))
             (incf carrier-y (aref +dy+ carrier-dir)))

    infection-count))

;; --- Main Entry Point ---

(defun main ()
  "Main entry point for the program.
   Reads input from 'input.txt' and prints the solution for Part 1 to standard output."
  (let* ((filename "input.txt")
         (bursts 10000) ; Number of bursts for Part 1
         (result (solve-part1 filename bursts)))
    (format t "~a~%" result)))

;; This line ensures that `main` is called when the script is loaded and executed.
;; For a compiled binary, a system-specific toplevel function would typically be configured.
(main)
