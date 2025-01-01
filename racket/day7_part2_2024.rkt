
#lang racket

(define (concat a b)
  (string->number (string-append (number->string a) (number->string b))))

(define (can-produce? target nums idx value)
  (cond
    [(= idx (length nums)) (= value target)]
    [else
     (let ([n (list-ref nums idx)])
       (or (can-produce? target nums (+ idx 1) (+ value n))
           (can-produce? target nums (+ idx 1) (* value n))
           (can-produce? target nums (+ idx 1) (concat value n))))]))

(define (solve)
  (let* ([lines (file->lines "input.txt")]
         [total (foldl (lambda (line acc)
                          (let* ([parts (string-split line ":")]
                                 [target (string->number (string-trim (first parts)))]
                                 [num-strs (string-split (string-trim (second parts)))])
                            (if (or (null? parts) (null? num-strs))
                                acc
                                (let* ([nums (map string->number num-strs)])
                                  (if (= (length nums) 1)
                                      (if (= (first nums) target)
                                          (+ acc target)
                                          acc)
                                      (if (can-produce? target nums 1 (first nums))
                                          (+ acc target)
                                          acc))))))
                       0
                       lines)])
    total))

(printf "~a\n" (solve))
