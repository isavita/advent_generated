#lang racket

(define (parse-chemical s)
  (let* ([parts (string-split s " ")]
         [amount (string->number (first parts))]
         [name (second parts)])
    (list name amount)))

(define (read-input filename)
  (define reactions (make-hash))
  (define ingredients (make-hash))
  (for ([line (in-lines (open-input-file filename))])
    (define parts (string-split line " => "))
    (define output (parse-chemical (second parts)))
    (define inputs (map parse-chemical (string-split (first parts) ", ")))
    (hash-set! reactions (first output) output)
    (hash-set! ingredients (first output) inputs))
  (values reactions ingredients))

(define (calculate-ore chem amount reactions ingredients surplus)
  (cond
    [(equal? chem "ORE") amount]
    [(>= (hash-ref surplus chem 0) amount)
     (hash-set! surplus chem (- (hash-ref surplus chem) amount))
     0]
    [else
     (set! amount (- amount (hash-ref surplus chem 0)))
     (hash-set! surplus chem 0)
     (define reaction (hash-ref reactions chem))
     (define times (ceiling (/ amount (second reaction))))
     (define ore (apply + (map (lambda (ingredient)
                                  (calculate-ore (first ingredient) (* (second ingredient) times) reactions ingredients surplus))
                                (hash-ref ingredients chem))))
     (hash-set! surplus chem (+ (hash-ref surplus chem 0) (- (* times (second reaction)) amount)))
     ore]))

(define-values (reactions ingredients) (read-input "input.txt"))
(define result (calculate-ore "FUEL" 1 reactions ingredients (make-hash)))
(displayln result)