
#lang racket

(require racket/port
         racket/vector
         racket/string)

;; Constants
(define GRID-SIZE 100)
(define NUM-STEPS 100)

;; --- Grid Representation ---
;; A grid is represented as a vector of vectors of booleans.
;; #t means "on", #f means "off".

;; --- Input Reading ---
;; Reads the initial grid configuration from a file.
(define (read-grid filename size)
  (with-input-from-file filename
    (lambda ()
      (for/vector ([i (in-range size)])
        (let ([line (read-line)])
          (for/vector ([j (in-range size)]
                       [char (in-string line)])
            (char=? char #\#)))))))

;; --- Helper: Check if coordinates are a corner ---
(define (is-corner? r c size)
  (or (and (= r 0) (= c 0))
      (and (= r 0) (= c (- size 1)))
      (and (= r (- size 1)) (= c 0))
      (and (= r (- size 1)) (= c (- size 1)))))

;; --- Helper: Force corners to be ON ---
;; Returns a *new* grid with corners forced to #t.
(define (force-corners-on grid size)
  (for/vector ([r (in-range size)]
               [row (in-vector grid)])
    (for/vector ([c (in-range size)]
                 [cell (in-vector row)])
      (if (is-corner? r c size)
          #t
          cell))))

;; --- Helper: Count ON neighbors ---
;; Counts the number of ON neighbors for a cell at (r, c).
;; Handles boundaries: cells outside the grid count as OFF.
(define (count-on-neighbors grid r c size)
  (for*/sum ([dr (in-range -1 2)]
             [dc (in-range -1 2)])
    (if (and (= dr 0) (= dc 0))
        0 ; Skip the center cell itself
        (let ([nr (+ r dr)]
              [nc (+ c dc)])
          (if (and (>= nr 0) (< nr size)
                   (>= nc 0) (< nc size))
              ;; Check if neighbor is within bounds AND is ON
              (if (vector-ref (vector-ref grid nr) nc) 1 0)
              ;; If neighbor is outside bounds, count as OFF (0)
              0)))))

;; --- Helper: Determine the next state of a cell ---
;; Based on its current state and the number of ON neighbors.
(define (next-state current-state num-on-neighbors)
  (if current-state ; If currently ON
      (or (= num-on-neighbors 2) (= num-on-neighbors 3))
      ; If currently OFF
      (= num-on-neighbors 3)))

;; --- Simulation Step (Part 1) ---
;; Computes the next state of the entire grid based on the current state.
;; All lights update simultaneously.
(define (simulate-step-part1 grid size)
  (for/vector ([r (in-range size)])
    (for/vector ([c (in-range size)])
      (let ([current-state (vector-ref (vector-ref grid r) c)]
            [num-on-neighbors (count-on-neighbors grid r c size)])
        (next-state current-state num-on-neighbors)))))

;; --- Simulation Step (Part 2) ---
;; Computes the next state, but forces corners to be ON.
(define (simulate-step-part2 grid size)
  (for/vector ([r (in-range size)])
    (for/vector ([c (in-range size)])
      (if (is-corner? r c size)
          #t ; Corners are always ON
          (let ([current-state (vector-ref (vector-ref grid r) c)]
                [num-on-neighbors (count-on-neighbors grid r c size)])
            (next-state current-state num-on-neighbors))))))

;; --- Main Simulation Loop ---
;; Runs the simulation for a given number of steps using a specified step function.
(define (simulate-steps initial-grid num-steps sim-fn size)
  (let loop ([step 0] [current-grid initial-grid])
    (if (= step num-steps)
        current-grid
        (loop (add1 step) (sim-fn current-grid size)))))

;; --- Output: Count total ON lights ---
(define (count-lights-on grid size)
  (for*/sum ([r (in-range size)]
             [c (in-range size)])
    (if (vector-ref (vector-ref grid r) c) 1 0)))

;; --- Main Entry Point ---
(define (main)
  ;; Part 1
  (define initial-grid-part1 (read-grid "input.txt" GRID-SIZE))
  (define final-grid-part1 (simulate-steps initial-grid-part1 NUM-STEPS simulate-step-part1 GRID-SIZE))
  (define count-part1 (count-lights-on final-grid-part1 GRID-SIZE))
  (printf "Part 1: ~a\n" count-part1)

  ;; Part 2
  ;; Start with the initial grid and force corners ON for the initial state
  (define initial-grid-part2-base (read-grid "input.txt" GRID-SIZE))
  (define initial-grid-part2-with-corners-on (force-corners-on initial-grid-part2-base GRID-SIZE))
  ;; Simulate using the Part 2 step function
  (define final-grid-part2 (simulate-steps initial-grid-part2-with-corners-on NUM-STEPS simulate-step-part2 GRID-SIZE))
  (define count-part2 (count-lights-on final-grid-part2 GRID-SIZE))
  (printf "Part 2: ~a\n" count-part2))

;; Run the main function
(main)
