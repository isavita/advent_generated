
#lang racket

(define Size 10007)

(define (main)
  (define deck (build-list Size identity))
  (define input (file->lines "input.txt"))
  
  (for ([line input])
    (cond
      [(string=? line "deal into new stack") (set! deck (deal-into-new-stack deck))]
      [(string-prefix? line "cut") 
       (set! deck (cut-n deck (string->number (second (string-split line))))) ]
      [(string-prefix? line "deal with increment") 
       (set! deck (deal-with-increment deck (string->number (last (string-split line))))) ]))
  
  (displayln (find-2019 deck)))

(define (deal-into-new-stack deck)
  (reverse deck))

(define (cut-n deck n)
  (if (>= n 0)
      (append (drop deck n) (take deck n))
      (append (drop deck (+ (length deck) n)) (take deck (+ (length deck) n)))))

(define (deal-with-increment deck n)
  (define new-deck (make-vector Size))
  (for ([i (in-range Size)])
    (vector-set! new-deck (modulo (* i n) Size) (list-ref deck i)))
  (vector->list new-deck))

(define (find-2019 deck)
  (for/fold ([index -1]) ([i (in-range Size)])
    (if (= (list-ref deck i) 2019)
        i
        index)))

(main)
