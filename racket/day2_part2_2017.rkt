
#lang racket

(define (read-file filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((lines '()))
        (let ((line (read-line)))
          (if (eof-object? line)
              (reverse lines)
              (loop (cons line lines))))))))

(define (parse-line line)
  (map string->number (string-split line)))

(define (checksum-part1 row)
  (let ((min (apply min row))
        (max (apply max row)))
    (- max min)))

(define (solve-part1 lines)
  (apply + (map (lambda (line) (checksum-part1 (parse-line line))) lines)))

(define (find-divisible row)
  (for*/first
   ((x row)
    (y row)
    #:when (and (not (= x y)) (zero? (remainder x y))))
   (/ x y)))

(define (solve-part2 lines)
  (apply + (map (lambda (line) (find-divisible (parse-line line))) lines)))

(define (main)
  (let ((lines (read-file "input.txt")))
    (printf "Part 1 Checksum: ~a\n" (solve-part1 lines))
    (printf "Part 2 Sum of Divisors: ~a\n" (solve-part2 lines))))

(main)
