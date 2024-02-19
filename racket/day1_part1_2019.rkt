#lang racket

; Function to calculate fuel required for a given mass
(define (fuel-required mass)
  (max 0 (- (floor (/ mass 3)) 2)))

; Function to read module masses from a file and calculate total fuel required
(define (total-fuel-required file)
  (with-input-from-file file
    (lambda ()
      (let loop ((sum 0))
        (let ((line (read-line)))
          (if (eof-object? line)
              sum
              (loop (+ sum (fuel-required (string->number line))))))))))

; Main function to solve the challenge
(define (solve-challenge file)
  (printf "Total fuel required: ~a\n" (total-fuel-required file)))

; Read from "input.txt" and print the answer
(solve-challenge "input.txt")
