
#lang racket

;; Define the dimensions of the screen
(define WIDTH 50)
(define HEIGHT 6)

;; Creates an initial screen of WIDTH x HEIGHT, with all pixels off (#f).
;; The screen is represented as a vector of vectors of booleans.
;; Each inner vector represents a row.
(define (make-screen)
  (build-vector HEIGHT (lambda (y) (build-vector WIDTH (lambda (x) #f)))))

;; Applies a 'rect AxB' operation.
;; Turns on all pixels in a rectangle at the top-left corner,
;; with dimensions width A and height B.
;; Dimensions are implicitly clipped by the screen size.
(define (apply-rect screen width height)
  ;; Ensure dimensions are within screen bounds, although puzzle input
  ;; typically provides valid dimensions for the specified screen size.
  (define actual-width (min width WIDTH))
  (define actual-height (min height HEIGHT))
  ;; Iterate through the rectangle's rows and columns and set pixels to #t
  (for* ([y (in-range actual-height)]
         [x (in-range actual-width)])
    (vector-set! (vector-ref screen y) x #t)))

;; Applies a 'rotate row y=A by B' operation.
;; Shifts all pixels in the specified row right by the given amount.
;; Pixels wrapping off the right edge appear on the left.
(define (apply-rotate-row screen row-index shift-by)
  ;; Ensure row-index is valid
  (when (and (>= row-index 0) (< row-index HEIGHT))
    (define row (vector-ref screen row-index))
    (define width (vector-length row)) ; This should be WIDTH
    ;; Create a new vector to store the rotated row pixels
    (define new-row (make-vector width #f))
    ;; Calculate the new position for each pixel in the row
    ;; A pixel at original column 'c' moves to new column '(c + shift-by) mod width'
    (for ([c (in-range width)])
      (define new-c (modulo (+ c shift-by) width))
      (vector-set! new-row new-c (vector-ref row c)))
    ;; Replace the old row vector in the screen with the new, rotated one
    (vector-set! screen row-index new-row)))

;; Applies a 'rotate column x=A by B' operation.
;; Shifts all pixels in the specified column down by the given amount.
;; Pixels wrapping off the bottom edge appear at the top.
(define (apply-rotate-column screen col-index shift-by)
  ;; Ensure col-index is valid
  (when (and (>= col-index 0) (< col-index WIDTH))
    (define height (vector-length screen)) ; This should be HEIGHT
    ;; Extract the current column values into a temporary vector
    (define column-values (make-vector height #f))
    (for ([r (in-range height)])
      (vector-set! column-values r (vector-ref (vector-ref screen r) col-index)))

    ;; Calculate the new column values after shifting, storing in another temporary vector
    (define new-column-values (make-vector height #f))
    ;; A pixel at original row 'r' moves to new row '(r + shift-by) mod height'
    (for ([r (in-range height)])
      (define new-r (modulo (+ r shift-by) height))
      (vector-set! new-column-values new-r (vector-ref column-values r)))

    ;; Update the screen with the new column values from the temporary vector
    (for ([r (in-range height)])
      (vector-set! (vector-ref screen r) col-index (vector-ref new-column-values r))))
  )

;; Parses an instruction string and applies the corresponding operation to the screen.
(define (process-instruction screen line)
  (cond
    ;; Match 'rect AxB'
    [(regexp-match #rx"rect ([0-9]+)x([0-9]+)" line)
     (match (regexp-match #rx"rect ([0-9]+)x([0-9]+)" line)
       [(list _ width-str height-str) ; Extract captured groups
        (define width (string->number width-str))
        (define height (string->number height-str))
        (apply-rect screen width height)])]
    ;; Match 'rotate row y=A by B'
    [(regexp-match #rx"rotate row y=([0-9]+) by ([0-9]+)" line)
     (match (regexp-match #rx"rotate row y=([0-9]+) by ([0-9]+)" line)
       [(list _ row-str shift-str) ; Extract captured groups
        (define row-index (string->number row-str))
        (define shift-by (string->number shift-str))
        (apply-rotate-row screen row-index shift-by)])]
    ;; Match 'rotate column x=A by B'
    [(regexp-match #rx"rotate column x=([0-9]+) by ([0-9]+)" line)
     (match (regexp-match #rx"rotate column x=([0-9]+) by ([0-9]+)" line)
       [(list _ col-str shift-str) ; Extract captured groups
        (define col-index (string->number col-str))
        (define shift-by (string->number shift-str))
        (apply-rotate-column screen col-index shift-by)])]
    [else
     ;; This case indicates an unexpected input format based on the puzzle description.
     (error "Unknown instruction format:" line)]))

;; Counts the number of lit pixels (#t) on the screen.
(define (count-lit screen)
  ;; Use nested iteration with for*/fold to sum up lit pixels
  (for*/fold ([count 0])             ; Initial count is 0
             ([row (in-vector screen)] ; Iterate through each row vector
              [pixel (in-vector row)]) ; Iterate through each pixel in the current row
    (if pixel (add1 count) count))) ; If the pixel is lit (#t), increment the count

;; Main function to execute the program logic:
;; 1. Initialize the screen.
;; 2. Read instructions from "input.txt".
;; 3. Process each instruction to modify the screen.
;; 4. Count the number of lit pixels.
;; 5. Print the count to standard output.
(define (main)
  ;; Initialize the screen state
  (define screen (make-screen))

  (define input-file "input.txt")

  ;; Read all lines from the input file.
  ;; file->lines is a convenient function for reading a whole file line by line.
  (define lines (file->lines input-file))

  ;; Process each instruction line obtained from the file
  (for ([line lines])
    (process-instruction screen line))

  ;; Count the number of lit pixels after all instructions have been applied
  (define lit-pixels (count-lit screen))

  ;; Print the final count of lit pixels to standard output
  (println lit-pixels))

;; Execute the main function when the script starts.
(main)
