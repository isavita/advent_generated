
#lang racket

(define (solve)
  (define safe-count 0)
  (with-input-from-file "input.txt"
    (lambda ()
      (for ([line (in-lines)])
        (define levels (map string->number (string-split line)))
        (when (safe? levels)
          (set! safe-count (+ safe-count 1))))))
  (printf "~a\n" safe-count))

(define (safe? levels)
  (cond
    [(< (length levels) 2) #f]
    [else
     (define first-diff (- (list-ref levels 1) (list-ref levels 0)))
     (cond
       [(= first-diff 0) #f]
       [else
        (define increasing? (> first-diff 0))
        (let loop ([i 0])
          (cond
            [(>= i (- (length levels) 1)) #t]
            [else
             (define diff (- (list-ref levels (+ i 1)) (list-ref levels i)))
             (cond
               [(= diff 0) #f]
               [(or (and increasing? (<= diff 0))
                    (and (not increasing?) (>= diff 0))) #f]
               [(let ([abs-diff (abs diff)])
                  (or (< abs-diff 1) (> abs-diff 3))) #f]
               [else (loop (+ i 1))])]))])]))

(solve)
