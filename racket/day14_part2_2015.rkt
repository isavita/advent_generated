
#lang racket

(define (parse-reindeer line)
  (define parts (string-split line " "))
  (list (string->number (list-ref parts 3))
        (string->number (list-ref parts 6))
        (string->number (list-ref parts 13))))

(define (distance-at-time reindeer time)
  (define speed (car reindeer))
  (define fly-time (cadr reindeer))
  (define rest-time (caddr reindeer))
  (define cycle-time (+ fly-time rest-time))
  (define full-cycles (quotient time cycle-time))
  (define remaining-time (remainder time cycle-time))
  (define flying-time (min remaining-time fly-time))
  (+ (* full-cycles fly-time speed) (* flying-time speed)))

(define (solve-part1 filename)
  (define lines (file->lines filename))
  (define reindeer-data (map parse-reindeer lines))
  (define distances (map (lambda (reindeer) (distance-at-time reindeer 2503)) reindeer-data))
  (apply max distances))

(define (solve-part2 filename)
  (define lines (file->lines filename))
  (define reindeer-data (map parse-reindeer lines))
  (define scores (make-vector (length reindeer-data) 0))
  (for ([time (in-range 1 2504)])
    (define distances (map (lambda (reindeer) (distance-at-time reindeer time)) reindeer-data))
    (define max-dist (apply max distances))
    (for ([i (in-range (length distances))])
      (when (= (list-ref distances i) max-dist)
        (vector-set! scores i (+ (vector-ref scores i) 1)))))
  (apply max (vector->list scores)))

(printf "Part 1: ~a\n" (solve-part1 "input.txt"))
(printf "Part 2: ~a\n" (solve-part2 "input.txt"))
