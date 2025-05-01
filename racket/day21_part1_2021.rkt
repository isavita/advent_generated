
#lang racket

(define (simulate p1-pos p2-pos p1-score p2-score die-sequence-num roll-count)
  (let* ([r1 (modulo die-sequence-num 100)]
         [r2 (modulo (+ die-sequence-num 1) 100)]
         [r3 (modulo (+ die-sequence-num 2) 100)]
         [rolls (+ r1 r2 r3)]
         [next-die-sequence-num (+ die-sequence-num 3)]
         [next-roll-count (+ roll-count 3)]
         [next-p1-pos (modulo (+ p1-pos rolls -1) 10)]
         [next-p1-pos-adj (+ next-p1-pos 1)]
         [next-p1-score (+ p1-score next-p1-pos-adj)])

    (if (>= next-p1-score 1000)
        (displayln (* p2-score next-roll-count))
        (let* ([r4 (modulo next-die-sequence-num 100)]
               [r5 (modulo (+ next-die-sequence-num 1) 100)]
               [r6 (modulo (+ next-die-sequence-num 2) 100)]
               [rolls2 (+ r4 r5 r6)]
               [next-die-sequence-num2 (+ next-die-sequence-num 3)]
               [next-roll-count2 (+ next-roll-count 3)]
               [next-p2-pos (modulo (+ p2-pos rolls2 -1) 10)]
               [next-p2-pos-adj (+ next-p2-pos 1)]
               [next-p2-score (+ p2-score next-p2-pos-adj)])

          (if (>= next-p2-score 1000)
              (displayln (* next-p1-score next-roll-count2))
              (simulate next-p1-pos-adj next-p2-pos-adj next-p1-score next-p2-score next-die-sequence-num2 next-roll-count2))))))

(define (main)
  (define input-file "input.txt")
  (with-input-from-file input-file
    (lambda ()
      (define line1 (read-line))
      (define line2 (read-line))

      (define p1-start (string->number (car (reverse (string-split line1 ": ")))))
      (define p2-start (string->number (car (reverse (string-split line2 ": ")))))

      (simulate p1-start p2-start 0 0 1 0))))

(main)
