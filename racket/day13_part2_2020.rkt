
#lang racket

(define (main)
  (define in (open-input-file "input.txt"))
  (read-line in)
  (define second-line (read-line in))
  (close-input-port in)

  (define bus-strings (string-split second-line ","))
  (define active-buses
    (for/list ((bus-str bus-strings) (idx (in-naturals)) #:unless (string=? bus-str "x"))
      (cons (string->number bus-str) idx)))

  (let loop ((current-t 0)
            (current-step 1)
            (bus-list active-buses))
    (if (null? bus-list)
        (displayln current-t)
        (let* ((bus-pair (car bus-list))
               (bus (car bus-pair))
               (offset (cdr bus-pair)))
          (let find-t ((t-candidate current-t))
            (if (zero? (modulo (+ t-candidate offset) bus))
                (loop t-candidate (* current-step bus) (cdr bus-list))
                (find-t (+ t-candidate current-step))))))))

(main)
