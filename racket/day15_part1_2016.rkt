
#lang racket

;; Represents a single disc
(struct disc (total-positions start-position) #:transparent)

;; Parses a line of input into a disc struct
(define (parse-line line)
  (match (regexp-match #px"Disc #\\d+ has (\\d+) positions; at time=0, it is at position (\\d+)\\." line)
    [(list _ total-str pos-str)
     (disc (string->number total-str) (string->number pos-str))]
    [_ (error 'parse-line "Invalid input format: ~a" line)]))

;; Checks if the capsule can pass through all discs at a given time
(define (check-time? time discs)
  (for/and ([d (in-list discs)] [i (in-naturals)]) ; in-naturals generates 0, 1, 2, ...
    (zero? (modulo (+ (disc-start-position d) time i 1)
                   (disc-total-positions d)))))

;; Finds the first time the capsule can pass
(define (find-first-time discs)
  (let loop ([time 0])
    (if (check-time? time discs)
        time
        (loop (add1 time)))))

;; Main entry point
(define (main)
  (define discs
    (call-with-input-file "input.txt"
      (lambda (port)
        (for/list ([line (in-lines port)])
          (parse-line line)))))
  
  (displayln (find-first-time discs)))

;; Run the program
(main)
