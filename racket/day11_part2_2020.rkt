#lang racket

(define (read-seats filename)
  (with-input-from-file filename
    (lambda ()
      (for/list ([line (in-lines)])
        (string->list line)))))

(define (count-visible-seats layout row col)
  (define directions '((-1 -1) (-1 0) (-1 1)
                       (0 -1)          (0 1)
                       (1 -1) (1 0) (1 1)))
  (define (first-visible-seat dr dc r c)
    (let loop ((r (+ r dr)) (c (+ c dc)))
      (cond
        [(or (< r 0) (>= r (length layout)) 
             (< c 0) (>= c (length (first layout)))) 0]
        [(char=? (list-ref (list-ref layout r) c) #\#) 1]
        [(char=? (list-ref (list-ref layout r) c) #\L) 0]
        [else (loop (+ r dr) (+ c dc))])))
  (apply + (map (lambda (d) (first-visible-seat (car d) (cadr d) row col))
                directions)))

(define (next-state layout)
  (define (next-seat r c)
    (let ((seat (list-ref (list-ref layout r) c))
          (adjacent-occupied (count-visible-seats layout r c)))
      (cond
        [(char=? seat #\L) (if (= adjacent-occupied 0) #\# seat)]
        [(char=? seat #\#) (if (>= adjacent-occupied 5) #\L seat)]
        [else seat])))
  (for/list ([r (in-range (length layout))])
    (for/list ([c (in-range (length (first layout)))])
      (next-seat r c))))

(define (seating-equilibrium layout)
  (define (stable? old new)
    (and (= (length old) (length new))
         (for/and ([r (in-range (length old))])
           (for/and ([c (in-range (length (first old)))])
             (char=? (list-ref (list-ref old r) c)
                      (list-ref (list-ref new r) c))))))
  (let loop ((current layout))
    (let ((next (next-state current)))
      (if (stable? current next)
          current
          (loop next)))))

(define (count-occupied layout)
  (apply + (map (lambda (row) (length (filter (lambda (seat) (char=? seat #\#)) row)))
                 layout)))

(define (main)
  (define initial-layout (read-seats "input.txt"))
  (define final-layout (seating-equilibrium initial-layout))
  (printf "Occupied seats: ~a\n" (count-occupied final-layout)))

(main)