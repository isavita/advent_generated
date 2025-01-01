
#lang racket

(define (mod a b)
  (remainder (+ (remainder a b) b) b))

(define (parse-line line)
  (let* ([matches (regexp-match #px"p=(-?\\d+),(-?\\d+) v=(-?\\d+),(-?\\d+)" line)]
         [x (string->number (list-ref matches 1))]
         [y (string->number (list-ref matches 2))]
         [vx (string->number (list-ref matches 3))]
         [vy (string->number (list-ref matches 4))])
    (list x y vx vy)))

(define (move-robots robots sizeX sizeY)
  (map (lambda (robot)
         (let* ([x (list-ref robot 0)]
                [y (list-ref robot 1)]
                [vx (list-ref robot 2)]
                [vy (list-ref robot 3)])
           (list (mod (+ x vx) sizeX)
                 (mod (+ y vy) sizeY)
                 vx
                 vy)))
       robots))

(define (count-quadrants robots sizeX sizeY)
  (let* ([centerX (quotient sizeX 2)]
         [centerY (quotient sizeY 2)]
         [counts (make-vector 4 0)])
    (for ([robot robots])
      (let* ([x (list-ref robot 0)]
             [y (list-ref robot 1)])
        (cond
          [(and (< x centerX) (< y centerY)) (vector-set! counts 0 (+ (vector-ref counts 0) 1))]
          [(and (< x centerX) (> y centerY)) (vector-set! counts 1 (+ (vector-ref counts 1) 1))]
          [(and (> x centerX) (< y centerY)) (vector-set! counts 2 (+ (vector-ref counts 2) 1))]
          [(and (> x centerX) (> y centerY)) (vector-set! counts 3 (+ (vector-ref counts 3) 1))])))
    counts))

(define (has-no-overlaps robots)
  (let ([positions (make-hash)])
    (for/and ([robot robots])
      (let ([pos (list (list-ref robot 0) (list-ref robot 1))])
        (if (hash-has-key? positions pos)
            #f
            (begin
              (hash-set! positions pos #t)
              #t))))))

(define (draw-grid robots sizeX sizeY)
  (let ([grid-map (make-hash)])
    (for ([robot robots])
      (hash-set! grid-map (list (list-ref robot 0) (list-ref robot 1)) #t))
    (for ([y (in-range sizeY)])
      (display (string-join (for/list ([x (in-range sizeX)])
                              (if (hash-has-key? grid-map (list x y))
                                  "#"
                                  "."))
                            ""))
      (newline))))

(define sizeX 101)
(define sizeY 103)

(define robots
  (with-input-from-file "input.txt"
    (lambda ()
      (for/list ([line (in-lines)]
                 #:when (not (string=? line "")))
        (parse-line line)))))

(define robots-part1 (map identity robots))
(for ([n (in-range 100)])
  (set! robots-part1 (move-robots robots-part1 sizeX sizeY)))
(define counts (count-quadrants robots-part1 sizeX sizeY))
(define safety-factor (apply * (vector->list counts)))
(printf "Part 1 - Safety Factor after 100 seconds: ~a\n" safety-factor)

(define robots-part2 (map identity robots))
(define seconds
  (let loop ([seconds 0])
    (if (has-no-overlaps robots-part2)
        seconds
        (let ([next-seconds (+ seconds 1)])
          (set! robots-part2 (move-robots robots-part2 sizeX sizeY))
          (if (> next-seconds 1000000)
              (begin
                (displayln "Exceeded maximum iterations without finding a unique position configuration.")
                (exit 1))
              (loop next-seconds))))))
(printf "Part 2 - Fewest seconds to display Easter egg: ~a\n" seconds)
(displayln "Final positions of robots:")
(draw-grid robots-part2 sizeX sizeY)
