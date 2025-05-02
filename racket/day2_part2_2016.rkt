
#lang racket

;; Define the keypads as lists of lists.
;; Use #f for positions that are not buttons.
(define keypad-part1
  '(( "1" "2" "3")
    ( "4" "5" "6")
    ( "7" "8" "9")))

(define keypad-part2
  '( (#f   #f  "1" #f   #f)
     (#f  "2"  "3" "4"  #f)
     ("5" "6"  "7" "8" "9")
     (#f  "A"  "B" "C"  #f)
     (#f   #f  "D" #f   #f)))

;; Helper function to get the button value at a specific (row, col)
;; Returns #f if the coordinates are out of bounds.
(define (get-button grid row col)
  (let ((num-rows (length grid)))
    (and (>= row 0) (< row num-rows)
         (let ((row-list (list-ref grid row)))
           (let ((num-cols (length row-list)))
             (and (>= col 0) (< col num-cols)
                  (list-ref row-list col)))))))

;; Helper function to find the starting position (row, col) of a button.
;; Assumes the button exists in the grid.
(define (find-start-position grid start-button)
  (let loop ((row 0) (rows grid))
    (let ((cols (car rows)))
      (let loop-cols ((col 0) (cels cols))
        (if (empty? cels)
            (loop (add1 row) (cdr rows))
            (if (equal? (car cels) start-button)
                (list row col)
                (loop-cols (add1 col) (cdr cels))))))))

;; Function to process a single move ('U', 'D', 'L', 'R')
;; Returns the new position if the move is valid, otherwise returns the current position.
(define (move current-pos grid move-char)
  (let ((current-row (first current-pos))
        (current-col (second current-pos)))
    (let ((next-pos
           (case move-char
             ((#\U) (list (sub1 current-row) current-col))
             ((#\D) (list (add1 current-row) current-col))
             ((#\L) (list current-row (sub1 current-col)))
             ((#\R) (list current-row (add1 current-col)))
             (else current-pos)))) ; Should not happen with valid input
      (let ((next-row (first next-pos))
            (next-col (second next-pos)))
        ;; Check if the potential next position is valid (within bounds and not #f)
        (if (get-button grid next-row next-col)
            next-pos  ; Valid move, return new position
            current-pos))))) ; Invalid move, return current position

;; Function to process a single line of instructions.
;; Starts at `start-pos` and applies each move in the `line`.
;; Returns the final position after all moves in the line are processed.
(define (process-line start-pos grid line)
  (for/fold ((current-pos start-pos))
            ((move-char (in-string line)))
    (move current-pos grid move-char)))

;; Function to solve one part of the puzzle.
;; Takes the grid, starting button value, and list of instruction lines.
;; Returns the final code as a string.
(define (solve-part grid start-button instructions)
  (let loop ((current-pos (find-start-position grid start-button))
             (remaining-instructions instructions)
             (code-digits '()))
    (if (empty? remaining-instructions)
        (string-append* (reverse code-digits))
        (let* ((line (car remaining-instructions))
               (final-pos-this-line (process-line current-pos grid line))
               (button-value (get-button grid (first final-pos-this-line) (second final-pos-this-line))))
          (loop final-pos-this-line
                (cdr remaining-instructions)
                (cons button-value code-digits))))))

;; Function to parse the input file into a list of instruction strings.
(define (parse-input filename)
  (with-input-from-file filename
    (lambda ()
      (port->lines (current-input-port)))))

;; Main execution block
(module+ main
  (define input-filename "input.txt")
  (define instructions (parse-input input-filename))

  ;; Part 1
  (define code1 (solve-part keypad-part1 "5" instructions))
  (printf "Part 1 Code: ~a~n" code1)

  ;; Part 2
  (define code2 (solve-part keypad-part2 "5" instructions))
  (printf "Part 2 Code: ~a~n" code2))
