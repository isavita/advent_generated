
;;; --- Hex Ed Challenge ---
;;;
;;; Implements a solution for Advent of Code Day 11: Hex Ed.
;;; It calculates the shortest path back to the origin on a hexagonal grid
;;; after following a given sequence of movements.
;;;
;;; Input is read from 'input.txt' and output is printed to standard output.

;; Define a helper function to split a string by a delimiter.
;; This is a common utility function as Lisp does not have a built-in string split.
(defun split-string (string delimiter)
  "Splits STRING by DELIMITER into a list of substrings."
  (loop for i = 0 then (1+ j)
        for j = (position delimiter string :start i)
        collect (subseq string i j)
        while j))

(defun calculate-hex-distance (steps)
  "Calculates the minimum steps to reach the end point given a list of hex moves.
   Uses cube coordinates (x, y, z) where x+y+z=0 to track position.
   The distance is the maximum of the absolute values of the coordinates."
  (let ((x 0) (y 0) (z 0)) ; Initialize starting position at (0, 0, 0)
    (dolist (step steps)
      (cond
        ((string= step "n")  (incf y) (decf z)) ; North
        ((string= step "s")  (decf y) (incf z)) ; South
        ((string= step "ne") (incf x) (decf z)) ; Northeast
        ((string= step "sw") (decf x) (incf z)) ; Southwest
        ((string= step "nw") (decf x) (incf y)) ; Northwest
        ((string= step "se") (incf x) (decf y)) ; Southeast
        (t (error "Unknown hex direction: ~a" step)))) ; Handle unknown directions
    
    ;; The distance in a hex grid using cube coordinates is max(abs(x), abs(y), abs(z)).
    (max (abs x) (abs y) (abs z))))

(defun main ()
  "Main entry point for the Hex Ed challenge.
   Reads the path from 'input.txt', calculates the minimum steps to origin,
   and prints the result to standard output."
  (let ((input-file "input.txt"))
    (with-open-file (in input-file :direction :input :if-does-not-exist :error)
      (let* ((line (read-line in nil nil)) ; Read the single line of input
             (steps (split-string line #\,)) ; Split the line into individual steps
             (distance (calculate-hex-distance steps))) ; Calculate the distance
        (format t "~a~%" distance))))) ; Print the final distance to standard output

;; Ensure the 'main' function is called when the script is loaded/run.
;; This is a common way to provide a program's entry point in Lisp scripts.
(main)
