
#lang racket

(define (read-input file)
  (with-input-from-file file
    (lambda ()
      (for/list ([line (in-lines)])
        (string->number line)))))

(define (find-groups weights target-group-weight)
  (define (group-sum group)
    (apply + group))
  
  (define (group-product group)
    (apply * group))
  
  (define (find-combinations weights target)
    (define (helper weights current-group)
      (cond
        [(= (group-sum current-group) target) (list current-group)]
        [(or (null? weights) (> (group-sum current-group) target)) '()]
        [else
         (let ([first (car weights)]
               [rest (cdr weights)])
           (append (helper rest (cons first current-group))
                   (helper rest current-group)))]))
    (helper weights '()))

  (define combinations (find-combinations weights target-group-weight))
  (filter (lambda (group)
            (and (not (null? group))
                 (= (group-sum group) target-group-weight)))
          combinations))

(define (quantum-entanglement group)
  (apply * group))

(define (ideal-configuration weights)
  (define total-weight (apply + weights))
  (define target-weight (/ total-weight 3))
  
  (define valid-groups (find-groups weights target-weight))
  
  (define (find-best-group groups)
    (define min-packages (apply min (map length groups)))
    (define filtered-groups (filter (lambda (g) (= (length g) min-packages)) groups))
    (define entanglements (map quantum-entanglement filtered-groups))
    (apply min entanglements))
  
  (find-best-group valid-groups))

(define (main)
  (define weights (read-input "input.txt"))
  (define result (ideal-configuration weights))
  (printf "The quantum entanglement of the first group in the ideal configuration is: ~a\n" result))

(main)
