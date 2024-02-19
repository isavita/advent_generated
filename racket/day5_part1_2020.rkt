
#lang racket

(define (binary-to-int binary-str)
  (define result 0)
  (for ([i (in-range (string-length binary-str))])
    (define char (string-ref binary-str i))
    (when (char=? char #\1)
      (set! result (bitwise-ior result (arithmetic-shift 1 (- (string-length binary-str) i 1))))))
  result)

(define (decode pass)
  (define row (binary-to-int (substring pass 0 7)))
  (define column (binary-to-int (substring pass 7)))
  (+ (* row 8) column))

(define (main)
  (define file (open-input-file "input.txt"))
  (define max-seat-id 0)
  (let loop ()
    (define line (read-line file))
    (unless (eof-object? line)
      (set! line (string-replace line "F" "0"))
      (set! line (string-replace line "B" "1"))
      (set! line (string-replace line "L" "0"))
      (set! line (string-replace line "R" "1"))
      (define seat-id (decode line))
      (when (> seat-id max-seat-id)
        (set! max-seat-id seat-id))
      (loop)))
  (close-input-port file)
  (displayln max-seat-id))

(main)
