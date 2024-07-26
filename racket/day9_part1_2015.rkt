
#lang racket

(define (main)
  (define distances (read-and-parse-input "input.txt"))
  (define locations (get-unique-locations distances))
  (define min-distance (find-shortest-route locations distances))
  (displayln min-distance))

(define (read-and-parse-input filename)
  (define distances (make-hash))
  (for ([line (in-lines (open-input-file filename))])
    (define parts (string-split line))
    (when (= (length parts) 5)
      (define from (list-ref parts 0))
      (define to (list-ref parts 2))
      (define distance (string->number (list-ref parts 4)))
      (hash-set! distances from (hash-set (hash-ref distances from (hash)) to distance))
      (hash-set! distances to (hash-set (hash-ref distances to (hash)) from distance))))
  distances)

(define (get-unique-locations distances)
  (hash-keys distances))

(define (find-shortest-route locations distances)
  (define (permute lst)
    (if (null? lst)
        (list (list))
        (apply append
               (map (lambda (x)
                      (map (lambda (p) (cons x p))
                           (permute (remove x lst))))
                    lst))))
  (define all-permutations (permute locations))
  (define (calculate-route-distance route)
    (for/sum ([i (in-range (sub1 (length route)))])
      (+ (hash-ref (hash-ref distances (list-ref route i)) (list-ref route (+ i 1))) 0)))
  (define min-distance
    (apply min (map calculate-route-distance all-permutations)))
  min-distance)

(main)
