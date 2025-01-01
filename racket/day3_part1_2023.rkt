
#lang racket

(define (read-file-to-matrix file-path)
  (with-input-from-file file-path
    (λ ()
      (for/list ([line (in-lines)])
        (string->list line)))))

(define (sum-of-part-numbers matrix)
  (define rows (length matrix))
  (define cols (length (car matrix)))
  (define visited (make-vector rows #f))
  (for ([i (in-range rows)])
    (vector-set! visited i (make-vector cols #f)))
  
  (define (extract-number x y)
    (let loop ([x x] [num-str '()])
      (cond
        [(or (>= x cols) (not (char-numeric? (list-ref (list-ref matrix y) x))))
         (values (string->number (list->string (reverse num-str))) (length num-str))]
        [else
         (loop (+ x 1) (cons (list-ref (list-ref matrix y) x) num-str))])))

  (define (check-adjacent x y)
    (for*/or ([dy (in-range -1 2)]
              [dx (in-range -1 2)]
              #:unless (and (= dy 0) (= dx 0)))
      (let ([adj-x (+ x dx)]
            [adj-y (+ y dy)])
        (and (>= adj-y 0) (< adj-y rows)
             (>= adj-x 0) (< adj-x cols)
             (let ([c (list-ref (list-ref matrix adj-y) adj-x)])
               (and (not (char-numeric? c)) (not (char=? c #\.))))))))

  (define (is-adjacent-to-symbol x y len)
    (for/or ([i (in-range len)])
      (check-adjacent (+ x i) y)))

  (let loop ([y 0] [sum 0])
    (cond
      [(= y rows) sum]
      [else
       (let loop2 ([x 0] [sum sum])
         (cond
           [(= x cols) (loop (+ y 1) sum)]
           [(and (not (vector-ref (vector-ref visited y) x)) (char-numeric? (list-ref (list-ref matrix y) x)))
            (let-values ([(num len) (extract-number x y)])
              (for ([i (in-range len)])
                (vector-set! (vector-ref visited y) (+ x i) #t))
              (if (is-adjacent-to-symbol x y len)
                  (loop2 (+ x len) (+ sum num))
                  (loop2 (+ x len) sum)))]
           [else (loop2 (+ x 1) sum)]))])))

(define matrix (read-file-to-matrix "input.txt"))
(displayln (sum-of-part-numbers matrix))
