#!/usr/bin/env scheme

(define (read-input filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((result '())
                 (line (read-line port)))
        (if (eof-object? line)
            (reverse result)
            (loop (cons (string-split line #\x) result) (read-line port)))))))

(define (string-split str delimiter)
  (string-split-helper str delimiter '()))

(define (string-split-helper str delimiter result)
  (let ((pos (string-index str delimiter)))
    (if pos
        (string-split-helper
         (substring str (+ pos 1) (string-length str))
         delimiter
         (cons (substring str 0 pos) result))
        (cons str result))))

(define (string-index str char)
  (let loop ((i 0))
    (cond ((= i (string-length str)) #f)
          ((char=? (string-ref str i) char) i)
          (else (loop (+ i 1))))))

(define (parse-dimensions dimensions)
  (map string->number dimensions))

(define (calculate-wrapping-paper l w h)
  (let* ((side1 (* l w))
         (side2 (* w h))
         (side3 (* h l))
         (smallest-side (min side1 side2 side3)))
    (+ (* 2 (+ side1 side2 side3)) smallest-side)))

(define (calculate-ribbon l w h)
  (let* ((perimeter1 (+ l l w w))
         (perimeter2 (+ w w h h))
         (perimeter3 (+ h h l l))
         (smallest-perimeter (min perimeter1 perimeter2 perimeter3))
         (volume (* l w h)))
    (+ smallest-perimeter volume)))

(define (main)
  (let* ((dimensions-list (read-input "input.txt"))
         (parsed-dimensions (map parse-dimensions dimensions-list))
         (total-wrapping-paper (apply + (map (lambda (dims) (apply calculate-wrapping-paper dims)) parsed-dimensions)))
         (total-ribbon (apply + (map (lambda (dims) (apply calculate-ribbon dims)) parsed-dimensions))))
    (display "Total square feet of wrapping paper: ")
    (display total-wrapping-paper)
    (newline)
    (display "Total feet of ribbon: ")
    (display total-ribbon)
    (newline)))

(main)