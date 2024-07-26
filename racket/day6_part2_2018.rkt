
#lang racket

(define (main)
  (define coordinates (parse-coordinates (file->string "input.txt")))
  (define region-size (find-region-size coordinates 10000))
  (displayln region-size))

(define (parse-coordinates input)
  (map (lambda (line)
         (match (string-split line ", ")
           [(list x y) (list (string->number x) (string->number y))]))
       (string-split (string-trim input) "\n")))

(define (find-region-size coordinates max-distance)
  (define-values (min-x min-y max-x max-y) (find-bounding-box coordinates))
  (define region-size 0)
  (for ([x (in-range min-x (+ max-x 1))])
    (for ([y (in-range min-y (+ max-y 1))])
      (when (< (total-distance x y coordinates) max-distance)
        (set! region-size (+ region-size 1)))))
  region-size)

(define (find-bounding-box coordinates)
  (define min-x (apply min (map first coordinates)))
  (define min-y (apply min (map second coordinates)))
  (define max-x (apply max (map first coordinates)))
  (define max-y (apply max (map second coordinates)))
  (values min-x min-y max-x max-y))

(define (total-distance x y coordinates)
  (apply + (map (lambda (c) (manhattan-distance x y (first c) (second c))) coordinates)))

(define (manhattan-distance x1 y1 x2 y2)
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(main)
