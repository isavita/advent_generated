
#lang racket

(define (parse-line line)
  (define parts (map (lambda (s) (string-split (substring s 3 (sub1 (string-length s))) ","))
                     (string-split line ", ")))
  (define p (map (lambda (lst) (map string->number lst)) parts))
  (list (list-ref p 0) (list-ref p 1) (list-ref p 2)))

(define (update-particle particle)
  (define p (car particle))
  (define v (cadr particle))
  (define a (caddr particle))
  (define new-v (map + v a))
  (define new-p (map + p new-v))
  (list new-p new-v a))

(define (simulate particles ticks)
  (for ([tick (in-range ticks)])
    (define positions (make-hash))
    (define updated-particles (map update-particle particles))
    (for ([particle updated-particles])
      (define pos (apply string-append (map (lambda (x) (format "~a" x)) (car particle))))
      (hash-set! positions pos (add1 (hash-ref positions pos 0))))
    (set! particles (filter (lambda (particle)
                              (= (hash-ref positions (apply string-append (map (lambda (x) (format "~a" x)) (car particle)))) 1))
                            updated-particles)))
  (length particles))

(define (main)
  (define input (file->lines "input.txt"))
  (define particles (map parse-line input))
  (printf "~a\n" (simulate particles 1000)))

(main)
