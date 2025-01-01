
#lang racket

(define (read-file filename)
  (file->lines filename))

(define (parse-input lines)
  (map (lambda (line)
         (map string->number (string-split line)))
       lines))

(define (all-zeros? nums)
  (andmap zero? nums))

(define (calculate-extrapolation history)
  (let loop ([lst history]
             [acc '()])
    (cond
      [(null? (cdr lst)) (reverse acc)]
      [else (loop (cdr lst) (cons (- (cadr lst) (car lst)) acc))])))

(define (calculate-extrapolations history)
  (let loop ([series (list history)])
    (let* ([prev (car series)]
           [extrap (calculate-extrapolation prev)])
      (if (all-zeros? prev)
          (reverse series)
          (loop (cons extrap series))))))

(define (solve lines)
  (let* ([histories (parse-input lines)]
         [predictions (map (lambda (history)
                             (let* ([extrapolations-series (calculate-extrapolations history)]
                                    [future-prediction (foldl (lambda (series acc)
                                                                 (+ (last series) acc))
                                                               0
                                                               extrapolations-series)])
                               future-prediction))
                           histories)])
    (apply + predictions)))

(let ([input (read-file "input.txt")])
  (displayln (solve input)))
