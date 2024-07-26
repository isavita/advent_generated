
#lang racket

(struct sensor (pos beacon dist))

(define (read-all path)
  (with-input-from-file path
    (lambda () (port->string (current-input-port)))))

(define (manhattan p q)
  (+ (abs (- (vector-ref p 0) (vector-ref q 0)))
     (abs (- (vector-ref p 1) (vector-ref q 1)))))

(define (impossible sensors y)
  (define pts (make-hash))
  (for ([s sensors])
    (define dist (- (sensor-dist s) (abs (- (vector-ref (sensor-pos s) 1) y))))
    (for ([x (in-range (add1 dist))])
      (hash-set! pts (+ (vector-ref (sensor-pos s) 0) x) #t)
      (hash-set! pts (- (vector-ref (sensor-pos s) 0) x) #t)))
  (for ([s sensors])
    (when (= (vector-ref (sensor-beacon s) 1) y)
      (hash-remove! pts (vector-ref (sensor-beacon s) 0))))
  (hash-count pts))

(define (parse-line line)
  (define regex (regexp "Sensor at x=([0-9-]+), y=([0-9-]+): closest beacon is at x=([0-9-]+), y=([0-9-]+)"))
  (define values (regexp-match regex line))
  (let ([pos (vector (string->number (second values)) (string->number (third values)))])
    (let ([beacon (vector (string->number (fourth values)) (string->number (fifth values)))])
      (sensor pos beacon (manhattan pos beacon)))))

(define (main)
  (define input (read-all "input.txt"))
  (define sensors (map parse-line (string-split input "\n")))
  (printf "~a\n" (impossible sensors 2000000)))

(main)
