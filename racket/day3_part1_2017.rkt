
#lang racket

(define (manhattan-distance target)
  (define side-length (if (odd? (ceiling (sqrt target))) 
                          (ceiling (sqrt target)) 
                          (+ 1 (ceiling (sqrt target)))))
  (define max-value (* side-length side-length))
  (define steps-from-edge (/ (- side-length 1) 2))
  (define distances (for/list ([i (in-range 4)])
                      (abs (- target (- max-value (* i (- side-length 1)) steps-from-edge)))))
  (+ steps-from-edge (apply min distances)))

(define input (string->number (string-trim (file->string "input.txt"))))
(printf "~a\n" (manhattan-distance input))
