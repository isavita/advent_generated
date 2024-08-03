(define (transform subject-number loop-size)
  (define (loop value count)
    (if (= count 0)
        value
        (loop (modulo (* value subject-number) 20201227) (- count 1))))
  (loop 1 loop-size))

(define (find-loop-size public-key)
  (define (loop value count)
    (if (= value public-key)
        count
        (loop (modulo (* value 7) 20201227) (+ count 1))))
  (loop 1 0))

(define (main)
  (define input (call-with-input-file "input.txt" (lambda (in)
    (list (read in) (read in)))))
  (define card-public-key (car input))
  (define door-public-key (cadr input))
  (define card-loop-size (find-loop-size card-public-key))
  (define encryption-key (transform door-public-key card-loop-size))
  (display encryption-key))

(main)