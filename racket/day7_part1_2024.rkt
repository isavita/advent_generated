
#lang racket

(define (can-produce? target nums idx current)
  (cond
    [(= idx (length nums)) (= current target)]
    [else (or (can-produce? target nums (+ idx 1) (+ current (list-ref nums idx)))
              (can-produce? target nums (+ idx 1) (* current (list-ref nums idx))))]))

(define (solve)
  (let* ([lines (file->lines "input.txt")]
         [total (foldl (lambda (line acc)
                          (let* ([parts (string-split line ":")]
                                 [target (string->number (string-trim (first parts)))]
                                 [num-strs (string-split (string-trim (second parts)))])
                            (if (null? num-strs)
                                acc
                                (let* ([nums (map string->number num-strs)])
                                  (if (and (= (length nums) 1) (= (first nums) target))
                                      (+ acc target)
                                      (if (can-produce? target nums 1 (first nums))
                                          (+ acc target)
                                          acc))))))
                       0
                       lines)])
    total))

(printf "~a\n" (solve))
