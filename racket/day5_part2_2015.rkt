
#lang racket

(define (passes-rule1 line)
  (define len (string-length line))
  (for/fold ([matches #f]) ([i (in-range (- len 2))])
    (define to-match (substring line i (+ i 2)))
    (for ([j (in-range (+ i 2) (- len 1))])
      (when (string=? (substring line j (+ j 2)) to-match)
        (set! matches #t)))
    matches))

(define (count-nice-strings input)
  (define nice 0)
  (for ([line (string-split input "\n")])
    (define rule1 (passes-rule1 line))
    (define rule2 (for/or ([i (in-range (- (string-length line) 2))])
                    (equal? (string-ref line i) (string-ref line (+ i 2)))))
    (when (and rule1 rule2)
      (set! nice (+ nice 1))))
  nice)

(define (main)
  (define input (string-trim (file->string "input.txt")))
  (define result (count-nice-strings input))
  (displayln result))

(main)
