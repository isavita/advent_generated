
#lang racket

(define (read-input filename)
  (with-input-from-file filename
    (lambda ()
      (read-line))))

(define (split-into-layers image width height)
  (define layer-size (* width height))
  (define layers (for/list ([i (in-range (quotient (string-length image) layer-size))])
                   (substring image (* i layer-size) (* (+ i 1) layer-size))))
  layers)

(define (count-digits layer)
  (define counts (make-vector 3 0)) ; counts for 0, 1, 2
  (for ([char (in-string layer)])
    (define digit (- (char->integer char) (char->integer #\0))) ; convert char to digit
    (when (and (>= digit 0) (<= digit 2))
      (vector-set! counts digit (+ 1 (vector-ref counts digit)))))
  counts)

(define (find-layer-with-fewest-zeros layers)
  (define (helper layers min-layer min-zeros)
    (if (null? layers)
        min-layer
        (let* ([layer (car layers)]
               [counts (count-digits layer)]
               [zeros (vector-ref counts 0)]
               [new-min-layer (if (< zeros min-zeros) layer min-layer)]
               [new-min-zeros (min zeros min-zeros)])
          (helper (cdr layers) new-min-layer new-min-zeros))))
  (helper layers "" +inf.0))

(define (calculate-product layer)
  (define counts (count-digits layer))
  (* (vector-ref counts 1) (vector-ref counts 2)))

(define (main)
  (define input (read-input "input.txt"))
  (define layers (split-into-layers input 25 6))
  (define layer-with-fewest-zeros (find-layer-with-fewest-zeros layers))
  (define result (calculate-product layer-with-fewest-zeros))
  (printf "The result is: ~a\n" result))

(main)
