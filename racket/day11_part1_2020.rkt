#lang racket

(define (read-input file)
  (with-input-from-file file
    (lambda ()
      (for/list ([line (in-lines)])
        (string->list line)))))

(define (adjacent-seats layout row col)
  (define directions '(-1 -1 -1 0 -1 1 0 -1 0 1 1 -1 1 0 1 1))
  (define (in-bounds? r c)
    (and (>= r 0) (< r (length layout))
         (>= c 0) (< c (length (list-ref layout 0)))))
  (for/fold ([count 0]) ([i (in-range 0 (length directions) 2)])
    (let* ([dr (list-ref directions i)]
           [dc (list-ref directions (+ i 1))]
           [nr (+ row dr)]
           [nc (+ col dc)])
      (if (and (in-bounds? nr nc)
               (eq? (list-ref (list-ref layout nr) nc) #\#))
          (+ count 1)
          count))))

(define (next-state layout)
  (for/list ([row (in-range (length layout))])
    (for/list ([col (in-range (length (list-ref layout row)))])
      (let ([seat (list-ref (list-ref layout row) col)]
            [adj (adjacent-seats layout row col)])
        (cond
          [(eq? seat #\L) (if (= adj 0) #\# seat)]
          [(eq? seat #\#) (if (>= adj 4) #\L seat)]
          [else seat])))))

(define (simulate layout)
  (define (loop current)
    (let ([next (next-state current)])
      (if (equal? current next)
          next
          (loop next))))
  (loop layout))

(define (count-occupied layout)
  (apply + (for/list ([row layout])
             (length (filter (lambda (seat) (eq? seat #\#)) row)))))

(define (main)
  (define layout (read-input "input.txt"))
  (define final-layout (simulate layout))
  (displayln (count-occupied final-layout)))

(main)