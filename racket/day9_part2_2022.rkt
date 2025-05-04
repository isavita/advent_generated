
#lang racket

(define (sign n)
  (cond
    ((> n 0) 1)
    ((< n 0) -1)
    (else 0)))

(define dir-map (hash
                 "U" (list 0 1)
                 "R" (list 1 0)
                 "D" (list 0 -1)
                 "L" (list -1 0)))

(define (next-point head tail)
  (define hx (first head))
  (define hy (second head))
  (define tx (first tail))
  (define ty (second tail))
  (if (and (<= (abs (- hx tx)) 1) (<= (abs (- hy ty)) 1))
      tail
      (list (+ tx (sign (- hx tx)))
            (+ ty (sign (- hy ty))))))

(define (solve input-lines ropelen)
  (define rope (make-vector ropelen (list 0 0)))
  (define visited (set))

  (for ([line input-lines])
    (define parts (string-split line))
    (when (= (length parts) 2)
      (define dir-char (first parts))
      (define steps (string->number (second parts)))
      (define move-vec (hash-ref dir-map dir-char))
      (define dx (first move-vec))
      (define dy (second move-vec))

      (for ([_ (in-range steps)])
        (define head-pos (vector-ref rope 0))
        (vector-set! rope 0 (list (+ (first head-pos) dx)
                                   (+ (second head-pos) dy)))

        (for ([j (in-range 1 ropelen)])
          (define prev-pos (vector-ref rope (sub1 j)))
          (define current-pos (vector-ref rope j))
          (vector-set! rope j (next-point prev-pos current-pos)))

        (set! visited (set-add visited (vector-ref rope (sub1 ropelen)))))))

  (set-count visited))

(define (main)
  (define input-path "input.txt")
  (define input-lines
    (with-input-from-file input-path
      (lambda () (port->lines (current-input-port)))))

  (define result (solve input-lines 10))

  (print result))

(main)
