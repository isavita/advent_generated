#lang racket

(define (main)
  (define asteroids (read-asteroids "input.txt"))
  (define max-count (find-best-asteroid-location asteroids))
  (displayln max-count))

(define (read-asteroids filename)
  (define lines (file->lines filename))
  (map (lambda (line) 
         (map (lambda (char) (equal? char #\#)) 
              (string->list line))) 
       lines))

(define (find-best-asteroid-location asteroids)
  (define (count-visible-asteroids x y)
    (define angles (make-hash))
    (for ([other-y (in-range (length asteroids))])
      (for ([other-x (in-range (length (list-ref asteroids other-y)))])
        (when (and (list-ref (list-ref asteroids other-y) other-x)
                   (not (and (= other-x x) (= other-y y))))
          (define angle (atan (- other-y y) (- other-x x)))
          (hash-set! angles angle #t))))
    (hash-count angles))
  
  (define max-count 0)
  (for ([y (in-range (length asteroids))])
    (for ([x (in-range (length (list-ref asteroids y)))])
      (when (list-ref (list-ref asteroids y) x)
        (define count (count-visible-asteroids x y))
        (set! max-count (max max-count count)))))
  max-count)

(main)