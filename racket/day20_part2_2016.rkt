
#lang racket

(require racket/string)

(define (main)
  (with-input-from-file "input.txt"
    (lambda ()
      (define data
        (sort
         (for/list ([line (in-lines)])
           (map string->number (string-split line "-")))
         (lambda (a b) (< (first a) (first b)))))

      (define max-ip 4294967295)

      (define-values (final-current-max final-allowed-ips)
        (foldl
         (lambda (range acc)
           (define start (first range))
           (define end (second range))
           (define current-max (first acc))
           (define allowed-ips (second acc))

           (define new-allowed-ips
             (if (> start (add1 current-max))
                 (+ allowed-ips (- start (add1 current-max)))
                 allowed-ips))

           (define new-current-max
             (max current-max end))

           (list new-current-max new-allowed-ips))
         (list 0 0)
         data))

      (define total-allowed-ips
        (+ final-allowed-ips (- max-ip final-current-max)))

      (print total-allowed-ips))))

(main)
