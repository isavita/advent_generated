#lang racket

(define (decode-seat boarding-pass)
  (define row-binary (substring boarding-pass 0 7))
  (define col-binary (substring boarding-pass 7 10))
  
  (define row (foldl (lambda (char acc)
                       (+ (* acc 2) (if (equal? char #\B) 1 0)))
                     0
                     (string->list row-binary)))
  
  (define col (foldl (lambda (char acc)
                       (+ (* acc 2) (if (equal? char #\R) 1 0)))
                     0
                     (string->list col-binary)))
  
  (+ (* row 8) col))

(define (find-highest-seat-id boarding-passes)
  (apply max (map decode-seat boarding-passes)))

(define (find-my-seat-id boarding-passes)
  (define seat-ids (sort (map decode-seat boarding-passes) <))
  (define all-ids (range (car seat-ids) (add1 (last seat-ids))))
  (define missing-id (for/fold ([result '()]) ([id all-ids])
                           (if (not (member id seat-ids))
                               (cons id result)
                               result)))
  (first missing-id))

(define (main)
  (define boarding-passes (file->lines "input.txt"))
  (define highest-id (find-highest-seat-id boarding-passes))
  (define my-seat-id (find-my-seat-id boarding-passes))
  
  (printf "Highest Seat ID: ~a\n" highest-id)
  (printf "My Seat ID: ~a\n" my-seat-id))

(main)