
#lang racket

(define (find-combinations containers target index count min-count ways)
  (cond
    [(= target 0)
     (if (= min-count 0)
         (values count 1)
         (if (< count min-count)
             (values count 1)
             (if (= count min-count)
                 (values min-count (+ ways 1))
                 (values min-count ways))))]
    [(or (< target 0) (>= index (length containers))) (values min-count ways)]
    [else
     (let-values ([(new-min new-ways) 
                   (find-combinations containers (- target (list-ref containers index)) (+ index 1) (+ count 1) min-count ways)])
       (let-values ([(final-min final-ways) 
                     (find-combinations containers target (+ index 1) count new-min new-ways)])
         (values final-min final-ways)))]))

(define (main)
  (define containers (map string->number (file->lines "input.txt")))
  (define-values (min-count ways) (find-combinations containers 150 0 0 0 0))
  (displayln ways))

(main)
