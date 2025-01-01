
#lang racket

(define (solve)
  (define grid (call-with-input-file "input.txt"
                (lambda (in)
                  (for/list ([line (in-lines in)])
                    line))))

  (define h (length grid))
  (define w (string-length (first grid)))
  (define antennas (make-hash))

  (for* ([y h]
         [x w])
    (define c (string-ref (list-ref grid y) x))
    (unless (char=? c #\.)
      (hash-update! antennas c (lambda (v) (cons (list y x) v)) '())))

  (define antinodes (make-hash))

  (for/list ([(k v) (in-hash antennas)])
    (define n (length v))
    (for ([i (in-range n)])
      (for ([j (in-range (+ i 1) n)])
        (define A (list-ref v i))
        (define B (list-ref v j))
        (define P1 (list (- (* 2 (first A)) (first B)) (- (* 2 (second A)) (second B))))
        (define P2 (list (- (* 2 (first B)) (first A)) (- (* 2 (second B)) (second A))))
        (when (and (>= (first P1) 0) (< (first P1) h) (>= (second P1) 0) (< (second P1) w))
          (hash-set! antinodes P1 #t))
        (when (and (>= (first P2) 0) (< (first P2) h) (>= (second P2) 0) (< (second P2) w))
          (hash-set! antinodes P2 #t)))))

  (printf "~a\n" (hash-count antinodes)))

(solve)
